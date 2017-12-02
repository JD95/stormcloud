{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}

module Config
  ( Environment(..)
  , Config(..)
  , DbConnStr
  , loadConfig
  ) where

import           Crypto.Saltine.Core.SecretBox (Key)
import           Data.Aeson
import           GHC.Generics
import           Options.Applicative
import qualified Prelude                       as P
import           Protolude

import           Action.HandShake
import Action.FileServer
import Action.Encryption

data Environment
  = Testing
  | Production
  | MockForClient
  | MockForStorage

newtype DbConnStr =
  DbConnStr Text
  deriving (Generic, ToJSON, FromJSON)

instance StringConv DbConnStr Text where
  strConv l (DbConnStr t) = t

instance P.Show DbConnStr where
  show (DbConnStr s) = P.show s

data Config = Config
  { dbConnStr  :: DbConnStr
  , ip         :: FileServerIp
  , port       :: FileServerPort
  , secretKey  :: Key
  } deriving (Generic, ToJSON, FromJSON)

testConfig = Config (DbConnStr "") (Ip "10.11.199.143") (Port 5555) <$> decodeBase16Key "D0E71F4118793D91894176C1BE7100EC85F34EC8DB25AD31FF69955E413CBAEE"


instance KeyRing Config where
  key = secretKey

instance FileServerConfig Config where
  fileServerIp = ip
  fileServerPort = port

newtype Options =
  Options Text

opts :: Text -> Parser Options
opts defaultConfig =
  Options <$>
  option
    auto
    (long "config" <> short 'c' <> metavar "CONFIG" <>
     help "The location of the server configuration" <>
     showDefault <>
     value defaultConfig)

optsInfo :: Text -> ParserInfo Options
optsInfo defaultConfig =
  info
    (opts defaultConfig <**> helper)
    (fullDesc <> progDesc "Launch the server" <> undefined "Raffle Assistant")

loadConfig e = do
  let filePath =
        case e of
          Testing        -> "test-config.json"
          MockForClient  -> "test-client-config.json"
          MockForStorage -> "test-storage-config.json"
          Production     -> "config.json"
  --(Options filePath) <- execParser (optsInfo filePath)
  decode @Config . toS <$> readFile ("config.json")

test = do
  print "Test"
  case testConfig of
    Just c -> do
        store "qwerty\r\nA71247DEDFFFFBAE" c
        print "Success"

