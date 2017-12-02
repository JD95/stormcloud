{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Action.FileServer
  ( FileServerIp(..)
  , FileServerPort(..)
  , FileServerConfig(..)
  , makeFileRequest
  ) where

import           Data.Aeson
import           Data.ByteString    (ByteString)
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

connectWithFileServer ::
     FileServerConfig a => a -> ((Socket, SockAddr) -> IO b) -> IO b
connectWithFileServer config f = do
  let (Ip ip) = fileServerIp config
  let (Port port) = fileServerPort config
  connect ip (show port) f

makeFileRequest ::
     (FileServerConfig config, KeyRing config)
  => config
  -> PlainText (Message ByteString)
  -> IO (Either String (PlainText (Message ByteString)))
makeFileRequest c request =
  connectWithFileServer c $ \(sock, addr) -> do
    sendMessage c request sock
    recvMessage c sock

handshake :: (FileServerConfig config, KeyRing config) => config -> IO Bool
handshake c =
  connectWithFileServer c $ \(sock, addr) -> do
    sendMessage c (toPlainText $ Message "handshake" "SCHWIFTY") sock
    response <- recvMessage c sock
    pure . flip (either (const False)) response $
      (==) (toPlainText $ Message "handshake" "WUBALUBADUBDUB")

emergency :: (FileServerConfig config, KeyRing config) => ByteString -> config -> IO ()
emergency m c =
  connectWithFileServer c $ \(sock, addr) -> do
    let msg = Message "header" $
                ("ALL YOUR BASE ARE BELONG TO US:" <> m)
    sendMessage c (toPlainText msg) sock

store :: (FileServerConfig config, KeyRing config) => ByteString -> config -> IO (Either String ())
store payload c =
  connectWithFileServer c $ \(sock, addr) -> do
    let msg = Message "header" $
                ("store\r\n" <> payload)
    sendMessage c (toPlainText msg) sock
    response <- recvMessage c sock
    pure $ flip fmap response $ \m -> undefined
