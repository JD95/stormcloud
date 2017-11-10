{-# LANGUAGE OverloadedStrings #-}

module Action.Keys
  ( CipherText
  , PlainText(..)
  , KeyRing(..)
  , sendCipherText
  , recvAndDecode
  ) where

import Control.Monad
import Control.Monad.Loops
import Crypto.Saltine.Core.Box
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Network.Simple.TCP

-- | Wrapper that prevents CipherText from being used elsewhere
newtype CipherText =
  CipherText ByteString

sendCipherText s (CipherText c) = send s c

recvAndDecode :: KeyRing a => a -> Socket -> IO (Either String PlainText)
recvAndDecode keyring s =
  maybe (Left "Error Recieving Message") (readMessage) . sequence <$>
  recieveMessage (recv s 2048)
  where
    readMessage = decrypt keyring . B.concat
    recieveMessage = unfoldWhileM (maybe False (B.isSuffixOf "\r\n\r\n"))

newtype PlainText =
  Plaintext ByteString
  deriving (Show)

-- | A type which holds the data needed
--   to communicate with the file server.
class KeyRing a
  -- | Attempt to verify and read a package sent from
  --   the file server.
                        where
  decrypt :: a -> ByteString -> Either String PlainText
  -- | Encrypt and sign package from file server.
  encrypt :: a -> PlainText -> CipherText
