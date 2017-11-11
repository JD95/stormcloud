{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Action.FileServer
  ( FileServerIp(..)
  , FileServerPort(..)
  , FileServerConfig(..)
  , FileServerMessage(..)
  , connectWithFileServer
  , packageMessage
  ) where

import Data.Aeson
import qualified Data.ByteString as B
import Data.List
import Data.Monoid
import GHC.Generics
import GHC.Natural
import Network.Simple.TCP

import Action.Keys

newtype FileServerIp =
  Ip String
  deriving (Show, Generic, ToJSON, FromJSON)

newtype FileServerPort =
  Port Natural
  deriving (Show, Generic, ToJSON, FromJSON)

class FileServerConfig a where
  fileServerIp :: a -> FileServerIp
  fileServerPort :: a -> FileServerPort

readServerKeys ::
     KeyRing a => B.ByteString -> B.ByteString -> B.ByteString -> Maybe a
readServerKeys = undefined

data FileServerMessage = Msg
  { requestType :: B.ByteString
  , payload :: B.ByteString
  }

packageMessage :: KeyRing a => a -> FileServerMessage -> CipherText
packageMessage keyring (Msg r p) =
  encrypt keyring . Plaintext . foldr1 (<>) . intersperse "\r\n" $
  [r, p, "\r\n"]

connectWithFileServer :: FileServerConfig a => a -> ((Socket, SockAddr) -> IO ()) -> IO ()
connectWithFileServer config f = do
  let (Ip ip) = fileServerIp config
  let (Port port) = fileServerPort config
  connect ip (show port) f 
