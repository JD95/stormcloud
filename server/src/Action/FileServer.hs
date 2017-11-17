{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Action.FileServer
  ( FileServerIp(..)
  , FileServerPort(..)
  , FileServerConfig(..)
  , connectWithFileServer
  ) where

import           Data.Aeson
import qualified Data.ByteString    as B
import           Data.List
import           Data.Monoid
import           GHC.Generics
import           GHC.Natural
import           Network.Simple.TCP

import           Action.Encryption

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

connectWithFileServer :: FileServerConfig a => a -> ((Socket, SockAddr) -> IO ()) -> IO ()
connectWithFileServer config f = do
  let (Ip ip) = fileServerIp config
  let (Port port) = fileServerPort config
  connect ip (show port) f
