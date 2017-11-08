module Action.Keys where

import Crypto.Saltine.Core.Box
import Data.ByteString

newtype CipherText 
  = CipherText ByteString

newtype PlainText
  = Plaintext ByteString

-- | A type which holds the data needed
--   to communicate with the file server.
class KeyRing a where
  
  -- | Attempt to verify and read a package sent from
  --   the file server.
  readFromFileServer :: a -> CipherText -> Maybe PlainText 

  -- | Encrypt and sign package from file server.
  writeForFileServer :: a -> PlainText -> CipherText 


