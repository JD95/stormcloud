{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Action.Encryption where
  -- ( KeyRing(..)
  -- , Message(..)
  -- , Base16
  -- , PlainText
  -- , toPlainText
  -- , encryptMessage
  -- , sendMessage
  -- , decryptMessage
  -- , recvMessage
  -- , fromBase16
  -- , toBase16
  -- , decodeBase16Key
  -- ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Crypto.Saltine.Class
import           Crypto.Saltine.Core.SecretBox
import           Crypto.Saltine.Internal.ByteSizes
import           Data.Aeson                        hiding (decode, encode)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Base16            as B16
import qualified Data.ByteString.Char8             as BC
import           Data.Char
import           Data.List                         hiding (head)
import           Data.Maybe
import           Network.Simple.TCP
import           Prelude                           hiding (head, undefined)
import           Protolude hiding (print)
import           System.Random

import           Action.Audit
import           Action.Base16

-- | A utility for decoding raw bytestrings as a "Key".
decodeBase16Key :: ByteString -> Maybe Key
decodeBase16Key = decode . fromBase16 . toBase16

-- | A type which holds the Secret Key
class KeyRing a where
  key :: a -> Key

instance FromJSON Key where
  parseJSON =
    withObject "secretKey" $ \o -> do
      k <- decodeBase16Key . B.pack <$> o .: "secretKey"
      maybe (fail "could not decode secret key") pure k

instance ToJSON Key where
  toJSON k = String (toS $ encode k)

class Encrypt a where
  encrypt :: KeyRing k => k -> a -> IO (Base16 a)
  decrypt :: KeyRing k => k -> Base16 a -> Either String a

instance Encrypt ByteString
  -- | Encrypts plaintext using a randomly generated nonce.
  -- Returns the encrypted message prefixed with the nonce used.
                                                                 where
  encrypt k b = do
    n <- newNonce
    pure (toBase16 $ encode n <> secretbox (key k) n b)
  -- | Decrypts message enocded in base16, prefixed with a nonce.
  decrypt k b =
    case decode n :: Maybe Nonce of
      Just nonce ->
        maybe (Left "Could not open box!") Right $ secretboxOpen (key k) nonce m
      Nothing -> Left "Could not decode nonce!"
    where
      (n, m) = B.splitAt secretBoxNonce $ fromBase16 b

-- | Uses a "Socket" to send a "Base16" string.
sendBase16 sock b = send sock . fromBase16 $ b

-- | Represents a message with a header and payload.
data Message content = Message
  { header  :: B.ByteString
  , payload :: content
  } deriving (Functor)

instance (Eq content) => Eq (Message content) where
  (Message a b) == (Message c d) = a == c && b == d

-- | Wraps content which is meant to be in plain text.
-- Does not define a Show instance nor does it export its
-- constructor so it is impossible to print.
newtype PlainText m =
  PlainText m

instance (Eq m) => Eq (PlainText m) where
  (PlainText a) == (PlainText b) = a == b

-- | Allows for types to be injected into PlainText.
toPlainText = PlainText

-- | Wraps content which is meant to be encrypted.
newtype CipherText m =
  CipherText m

-- | Encrypts the payload of a "PlainText" message. Prefixes the payload with the nonce
-- used to make it.
encryptMessage ::
     (Encrypt b, KeyRing k)
  => k
  -> PlainText (Message b)
  -> IO (CipherText (Message (Base16 b)))
encryptMessage key (PlainText msg) =
  CipherText . Message (header msg) <$> encrypt key (payload msg)

-- | Decrypts the payload of a "CipherText" message. Expects the payload to be in base 16
-- with the nonce used to create it at the front of the message.
decryptMessage ::
     (Encrypt b, KeyRing k)
  => k
  -> CipherText (Message (Base16 b))
  -> Either String (PlainText (Message b))
decryptMessage key (CipherText msg) =
  PlainText . Message (header msg) <$> decrypt key (payload msg)

-- | Encrypts a message using the key. Message is prefixed with the given header.
-- they payload is prefixed with the nonce used to generate the message.
-- The whole package is ended with \r\n\r\n.
sendMessage ::
     (KeyRing k) => k -> PlainText (Message ByteString) -> Socket -> IO ()
sendMessage key msg sock = do
  CipherText cipher <- encryptMessage key msg
  let (Base16 b) = (B.append (header cipher <> "\r\n") . (`B.append` "\r\n\r\n")) <$> payload cipher
  send sock b


-- | Converts a predicate into a maybe result.
maybePred :: (a -> Bool) -> (a -> Maybe a)
maybePred f a
  | f a = Just a
  | otherwise = Nothing

-- | Recieve on a socket, returning Nothing if the double carrage return is detected.
recvUntilEnd :: Socket -> Int -> IO [ByteString]
recvUntilEnd s i = do
  r <- recv s i
  case r of
    Just r' ->
      if "\r\n\r\n" `B.isSuffixOf` r'
        then pure [r']
        else (r' :) <$> recvUntilEnd s i
    Nothing -> pure []

-- | Breaks a message up on a carrage return.
breakReturn :: ByteString -> Maybe (ByteString, ByteString)
breakReturn "" = Nothing
breakReturn "\r\n" = Nothing
breakReturn b = Just (m, B.drop (B.length "\r\n") n)
  where
    (m, n) = B.breakSubstring "\r\n" b

-- | Attempts to parse out the header and payload of an encrypted message.
parseHeaderAndPayload ::
     ByteString -> Either String (CipherText (Message (Base16 ByteString)))
parseHeaderAndPayload b =
  case unfoldr breakReturn b of
    [header, content] ->
      Right . CipherText $ Message header (toBase16 content)
    (_:_:_:_) -> Left "Could not parse header and content. Too many sections"
    _ -> Left "Could not parse header and content. Too few sections"

-- | Recieves a message with a header, encrypted payload prefixed by the nonce used to make it,
-- and ended with \r\n\r\n.
recvMessage ::
     (KeyRing k)
  => k
  -> Socket
  -> IO (Either String (PlainText (Message ByteString)))
recvMessage key s = do
  p <- recvUntilEnd s 2048
  print p
  pure $ ((decryptMessage key <=< parseHeaderAndPayload) . B.concat) p 

verifyResponse ::
     ByteString
  -> CommandHistory
  -> PlainText (Message ByteString)
  -> Either ByteString ()
verifyResponse header c (PlainText (Message h payload)) = do
  when (header /= h) $
    (Left $ "Recieved header " <> h <> " when expected " <> header)
  maybe (Left "Response did not contain history")
    (flip verifyCommandHistory c)
    (snd <$> breakReturn payload)

testVerifyResponse :: IO ()
testVerifyResponse = do
  g <- newStdGen
  let header = "something"
  let c@ (CommandHistory s _) = fst $ random g
  undefined
