{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.HandShake
  ( FileServerIp
  , FileServerPort
  , FileServerConfig(..)
  ) where

import Control.Monad.Loops
import Crypto.Saltine.Class
import Crypto.Saltine.Core.SecretBox
import Crypto.Saltine.Internal.ByteSizes
import Data.Aeson hiding (decode, encode)
import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import GHC.Generics
import GHC.Natural
import Network.Simple.TCP
import Text.Read

import Action.FileServer
import Action.JsonParse
import Action.Keys

data Test = Test
  { testIp :: String
  , testPort :: Natural
  , secretKey :: Key
  }

objectValue name value =
  quote *> symbol name *> quote *> colon *> value <* comma

parseTest :: Parser Test
parseTest = openCurly *> content <* closedCurly
  where
    content =
      Test <$> (BC.unpack <$> objectValue "testIp" string) <*>
      objectValue "testPort" int <*>
      (fmap (fromJust . decodeBase16Key) $ objectValue "secretKey" string)

readTestConfig :: IO (Either String ([ByteString], Test))
readTestConfig = parse parseTest <$> B.readFile "test-config.json"

instance KeyRing Test where
  key = secretKey

instance FileServerConfig Test where
  fileServerIp = Ip . testIp
  fileServerPort = Port . testPort

test = do
  n <- newNonce
  config <- readTestConfig
  flip (either print) config $ \(_, t) ->
    connectWithFileServer t $ \(sock, addr) -> do
      sendMessage t (toPlainText $ Msg "hello" "test") sock
      response <- recvMessage @Test @FileServerMessage t sock
      flip (either print) response $ \p ->
        if p == (toPlainText $ Msg "random" "swifty")
          then print "Success!"
          else print "Failure!"

