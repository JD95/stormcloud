{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Action.HandShake
  ( FileServerIp
  , FileServerPort
  , FileServerConfig(..)
  , handshake
  ) where

import           Crypto.Saltine.Core.Box
import           Data.Aeson
import qualified Data.ByteString as B
import           Data.List
import           Data.Monoid
import           GHC.Generics
import           GHC.Natural
import           Network.Simple.TCP
import Control.Monad.Loops

newtype FileServerIp
  = Ip String
    deriving (Show, Generic, ToJSON, FromJSON)

newtype FileServerPort
  = Port Natural
    deriving (Show, Generic, ToJSON, FromJSON)

class FileServerConfig a where
  fileServerIp :: a -> FileServerIp
  fileServerPort :: a -> FileServerPort

data FileServerMessage
  = Msg
  { requestType :: B.ByteString
  , payload :: B.ByteString
  } 

toByteString :: FileServerMessage -> B.ByteString
toByteString (Msg r p) = foldr1 (<>) . intersperse "\r\n" $ [r,p,"\r\n"] 

data Test = Test String Natural

instance FileServerConfig Test where
  fileServerIp (Test r _) = Ip r
  fileServerPort (Test _ p) = Port p

recieveResponse s = do
  r <- recv s 2048
  case r of
    Nothing -> pure ""
    Just b -> if B.isSuffixOf "\r\n\r\n" b
                 then pure b
                 else fmap (b <>) (recieveResponse s) 

handshake config = do
  let (Ip ip) = fileServerIp config
  let (Port port) = fileServerPort config
  connect ip (show port) $ \(sock, addr) -> do
    let msg1 = Msg "upload" "hello"
    let msg2 = Msg "download" "stuff"
    send sock (toByteString msg1)
    response <- recieveResponse sock 
    print response
    send sock (toByteString msg2)
    response2 <- recieveResponse sock 
    print response2
