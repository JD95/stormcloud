{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.HandShake
  ( FileServerIp
  , FileServerPort
  , FileServerConfig(..)
  ) where

import           Control.Monad.Loops
import           Crypto.Saltine.Class
import           Crypto.Saltine.Core.SecretBox
import           Data.Aeson              hiding (encode, decode)
import           Data.ByteString
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as BC
import           Data.List
import           Data.Monoid
import           GHC.Generics
import           GHC.Natural
import           Network.Simple.TCP
import           Text.Read
import Data.Maybe
import qualified Data.ByteString.Base16 as B16
import Data.Char
import Crypto.Saltine.Internal.ByteSizes

import           Action.FileServer
import           Action.Keys
import           Action.JsonParse

data Test = Test
  { testIp     :: String
  , testPort   :: Natural
  , secretKey  :: Key 
  }

objectValue name value = quote *> symbol name *> quote *> colon *> value <* comma

fromBase16 = fst . B16.decode . BC.map toLower
toBase16 = BC.map toUpper . B16.encode

parseTest :: Parser Test
parseTest = openCurly *> content <* closedCurly 
  where content =
          Test <$> (BC.unpack <$> objectValue "testIp" string)
          <*> objectValue "testPort" int
          <*> (fmap (fromJust . decode . fromBase16) $ objectValue "secretKey" string)

readTestConfig :: IO (Either String ([ByteString], Test))
readTestConfig = parse parseTest <$> B.readFile "test-config.json"

packageMessage :: Test -> Nonce -> FileServerMessage -> ByteString 
packageMessage t n (Msg r p) =
  toBase16 . secretbox (secretKey t) n . Data.List.foldr1 (<>) . Data.List.intersperse "\r\n" $ [r, p, "\r\n"]

recvAndDecode :: Test -> Socket -> IO (Maybe ByteString) 
recvAndDecode t s = do
  -- maybe (Left "Error Recieving Message") (readMessage) . sequence <$>
   msg <- B.concat  <$> recieveMessage (recv s 2048)
   let (n, m') = B.splitAt (secretBoxNonce) msg
   let m = (fromBase16 . BC.filter (\b -> b /= '\r' || b /= '\n') $ m')
   print n
   print m
   case decode (fromBase16 n) :: Maybe Nonce of
     Just nonce -> pure $ readMessage nonce m 
     Nothing -> pure $ pure "Failed!"
   where
     readMessage n = secretboxOpen (secretKey t) n
     recieveMessage :: IO (Maybe ByteString) -> IO [ByteString]
     recieveMessage m = do
       result <- m
       case result of
         Just r -> if "\r\n\r\n" `B.isSuffixOf` r
           then pure [r]
           else fmap (r:) (recieveMessage m)
         Nothing -> pure [""]  

test = do
  n <- newNonce
  config <- readTestConfig
  case config of
    Right (_, t) -> do
      m <- pure (packageMessage t n (Msg "hello" "test"))
      connect "10.11.199.143" "5555" $ \(sock, addr) -> do
        send sock ((toBase16 $ encode n) <> m)
        print =<< recvAndDecode t sock 
    Left e -> print e

instance FileServerConfig Test where
  fileServerIp = Ip . testIp
  fileServerPort = Port . testPort

-- handshake :: (KeyRing a, FileServerConfig a) => a -> IO ()
-- handshake config =
--   connectWithFileServer config $ \(sock, addr) -> do
--     let msg1 = Msg "upload" "hello"
--     sendCipherText sock (packageMessage config msg1)
--     print =<< recvAndDecode config sock
--     let msg2 = Msg "download" "stuff"
--     sendCipherText sock (packageMessage config msg2)
--     print =<< recvAndDecode config sock
