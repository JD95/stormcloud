{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Action.FileServer where
  -- ( FileServerIp(..)
  -- , FileServerPort(..)
  -- , FileServerConfig(..)
  -- , makeFileRequest
  -- , store
  -- ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import           Data.List
import           Data.Monoid
import           GHC.Generics
import Data.Sequence
import           GHC.Natural
import           Network.Simple.TCP

import           Action.Audit
import           Action.Encryption
import           Error

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

testConnect :: IO ()
testConnect = do
  connect "10.11.199.143" "5555" $ \(sock, addr) -> do
    send sock "store\r\nhello!\r\n\r\n"

makeFileRequest ::
     (FileServerConfig config, KeyRing config)
  => config
  -> PlainText (Message ByteString)
  -> IO (Either ErrorMessage (PlainText (Message ByteString)))
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

emergency ::
     (FileServerConfig config, KeyRing config) => config -> ByteString-> IO ()
emergency c m=
  connectWithFileServer c $ \(sock, addr) -> do
    let msg = Message "header" $ ("ALL YOUR BASE ARE BELONG TO US:" <> m)
    sendMessage c (toPlainText msg) sock

correctHeader :: ByteString -> Message ByteString -> STM (Validation ())
correctHeader header (Message h payload)
  | header == h = pure $ pure ()
  | otherwise =
    (pure . Left . ErrorMessage $
     "Recieved header " <> h <> " when expected " <> header)

syncedCommandHistory ::
     (Audit config)
  => config
  -> Action StorageAction 
  -> Message ByteString
  -> STM (Validation ())
syncedCommandHistory c a (Message h payload) = do
  cmdHist <- readTVar $ cmdHistory c
  let cmdHist' = cmdHist { storageActions = a <| (storageActions cmdHist)}
  writeTVar (cmdHistory c) cmdHist' 
  pure $
    maybe
      (Left . ErrorMessage $ "Response did not contain history")
      (flip verifyCommandHistory cmdHist)
      (snd <$> breakReturn payload)

checkFor :: Monad m => [a -> m (Validation ())] -> a -> m (Validation ())
checkFor validations input =
  foldM_ (flip const) () <$> mapM ($ input) validations

store ::
     (Audit state, FileServerConfig state, KeyRing state)
  => state
  -> Action StorageAction
  -> ByteString
  -> IO (Validation ())
store c a =
  storageCommand a c $
  checkFor [syncedCommandHistory c a, correctHeader "store"]

delete ::
  (Audit state, FileServerConfig state, KeyRing state)
  => state
  -> Action StorageAction
  -> ByteString
  -> IO (Validation ())
delete c a =
  storageCommand a c $
  checkFor [syncedCommandHistory c a, correctHeader "delete"]

retrieve ::
  (Audit state, FileServerConfig state, KeyRing state)
  => state
  -> Action StorageAction
  -> ByteString
  -> IO (Validation ())
retrieve c a =
  storageCommand a c $
  checkFor [syncedCommandHistory c a, correctHeader "retrieve"]

newtype Command =
  Command ByteString

storageCommand ::
     (Audit state, FileServerConfig state, KeyRing state)
  => Action StorageAction 
  -> state
  -> (Message ByteString -> STM (Validation a))
  -> ByteString
  -> IO (Validation a)
storageCommand a c v payload =
  connectWithFileServer c $ \(sock, addr) -> do
    let msg = Message (toHeader a) payload
    sendMessage c (toPlainText msg) sock
    response <- recvMessage c sock
    either (pure . Left) (atomically . (`validateWith` v)) response

