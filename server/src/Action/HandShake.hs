{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Action.HandShake
  ( FileServerIp
  , FileServerPort
  , FileServerConfig(..)
  , handshake
  ) where

import Control.Monad.Loops
import Crypto.Saltine.Class
import Crypto.Saltine.Core.Box
import Data.Aeson hiding (decode)
import qualified Data.ByteString as B
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Monoid
import GHC.Generics
import GHC.Natural
import Network.Simple.TCP
import Text.Read

import Action.FileServer
import Action.Keys

data Test = Test
  { testIp :: String
  , testPort :: Natural
  , testPubKey :: PublicKey
  , testSecKey :: SecretKey
  }

parseTest :: [ByteString] -> Maybe Test
parseTest [inputIp, inputPort, inputPub, inputSec] =
  Test <$> (readMaybe . BC.unpack $ inputIp) <*>
  (readMaybe . BC.unpack $ inputPort) <*>
  decode inputPub <*>
  decode inputSec
parseTest _ = Nothing

readTestConfig :: IO (Maybe Test)
readTestConfig = parseTest . BC.lines <$> B.readFile "test-config.txt"

instance FileServerConfig Test where
  fileServerIp = Ip . testIp
  fileServerPort = Port . testPort

handshake :: (KeyRing a, FileServerConfig a) => a -> IO ()
handshake config =
  connectWithFileServer config $ \(sock, addr) -> do
    let msg1 = Msg "upload" "hello"
    sendCipherText sock (packageMessage config msg1)
    print =<< recvAndDecode config sock
    let msg2 = Msg "download" "stuff"
    sendCipherText sock (packageMessage config msg2)
    print =<< recvAndDecode config sock
