{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Encryption
  ( KeyRing(..)
  , Message(..)
  , Base16
  , toPlainText
  , encryptMessage
  , sendMessage
  , decryptMessage
  , recvMessage
  , fromBase16
  , toBase16
  , decodeBase16Key
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Crypto.Saltine.Class
import Crypto.Saltine.Core.SecretBox
import Crypto.Saltine.Internal.ByteSizes
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.List
import Data.Monoid
import Network.Simple.TCP

newtype Base16 a =
  Base16 a

class Base16Conv a where
  fromBase16 :: Base16 a -> a
  toBase16 :: a -> Base16 a

instance (Monoid m) => Monoid (Base16 m) where
  mempty = Base16 mempty 
  mappend (Base16 a) (Base16 b) = Base16 (a `mappend` b)

instance Base16Conv ByteString where
  -- | A utility for reading encrypted messages.
  --  Converts text from uppercase base 16 encoding.
  fromBase16 (Base16 b) = fst . B16.decode . BC.map toLower $ b
  
  -- | A utility for creating encrypted messages.
  --  Converts text to uppercase base 16 encoding.
  toBase16 = Base16 . BC.map toUpper . B16.encode

-- | A utility for decoding raw bytestrings as a "Key".
decodeBase16Key :: ByteString -> Maybe Key
decodeBase16Key = decode . fromBase16 . Base16

-- | Lifts a function on a "ByteString" into a function on "Base16".
base16 :: (a -> a) -> (Base16 a -> Base16 a)
base16 f (Base16 b) = Base16 (f b)

-- | A type which holds the Secret Key
class KeyRing a where
  key :: a -> Key

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

  decrypt k (Base16 b) =
    case decode n :: Maybe Nonce of
      Just nonce ->
        maybe (Left "Could not open box!") Right $ secretboxOpen (key k) nonce m
      Nothing -> Left "Could not decode nonce!"
    where
      (n, m) = B.splitAt secretBoxNonce b

-- | Uses a "Socket" to send a "Base16" string.
sendBase16 sock (Base16 b) = send sock b

-- | Represents a message with a header and payload.
class Message m content where
  header :: m content -> content
  payload :: m content -> content
  buildMessage :: conent -> content -> m content

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
     (Encrypt b, KeyRing k, Message m b, Message m (Base16 b)) => k -> PlainText (m b) -> IO (CipherText (m (Base16 b)))
encryptMessage key (PlainText msg) =
  CipherText . buildMessage (header msg) <$> encrypt key (payload msg)

-- | Decrypts the payload of a "CipherText" message. Expects the payload to be in base 16
-- with the nonce used to create it at the front of the message.
decryptMessage ::
     (Encrypt b, KeyRing k, Message m b, Message m (Base16 b))
  => k
  -> CipherText (m (Base16 b))
  -> Either String (PlainText (m b))
decryptMessage key (CipherText msg) =
  PlainText . buildMessage (header msg) <$>
  decrypt key (payload msg)

-- | Encrypts a message using the key. Message is prefixed with the given header.
-- they payload is prefixed with the nonce used to generate the message.
-- The whole package is ended with \r\n\r\n.
sendMessage ::
     (KeyRing k, Message m ByteString) => k -> PlainText (m ByteString) -> Socket -> IO ()
sendMessage key msg sock = do
  CipherText cipher <- encryptMessage key msg
  sendBase16 sock $
    base16 (B.append (header cipher <> "\r\n") . (`B.append` "\r\n\r\n")) .
    payload cipher

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
     Message m ByteString
  => ByteString
  -> Either String (CipherText (m ByteString))
parseHeaderAndPayload b
  | length sections /= 2 =
    Left "Could not parse header and content. Too many sections"
  | otherwise =
    Right . CipherText $ buildMessage (head sections) (head . tail $ sections)
  where
    sections = unfoldr breakReturn b

-- | Recieves a message with a header, encrypted payload prefixed by the nonce used to make it,
-- and ended with \r\n\r\n.
recvMessage ::
     (KeyRing k, Message m ByteString)
  => k
  -> Socket
  -> IO (Either String (PlainText (m ByteString)))
recvMessage key s =
  (decryptMessage key <=< parseHeaderAndPayload) . B.concat <$>
  recvUntilEnd s 2048
