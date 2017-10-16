{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Environment(..)
  , Config(..)
  , DbConnStr
  , AppJs
  , StripeSecret
  , loadConfig
  ) where

import           Data.Aeson
import           GHC.Generics
import qualified Prelude      as P
import           Protolude
import Options.Applicative

data Environment = Testing | Production

newtype DbConnStr =
  DbConnStr Text
  deriving (Generic, ToJSON, FromJSON)

instance StringConv DbConnStr Text where
  strConv l (DbConnStr t) = t

instance P.Show DbConnStr where
  show (DbConnStr s) = P.show s

newtype AppJs =
  AppJs Text
  deriving (Generic, ToJSON, FromJSON)

instance StringConv AppJs Text where
  strConv l (AppJs t) = t

instance P.Show AppJs where
  show (AppJs s) = P.show s

newtype StripeSecret =
  StripeSecret Text
  deriving (Generic, ToJSON, FromJSON)

instance StringConv StripeSecret Text where
  strConv l (StripeSecret t) = t

instance P.Show StripeSecret where
  show (StripeSecret s) = P.show s

data Config = Config
  { dbConnStr :: DbConnStr
  , appJs :: AppJs
  , stripeSecret :: StripeSecret
  , prefillStr :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype Options = Options Text

opts :: Text -> Parser Options
opts  defaultConfig =
  Options <$> option auto
    ( long "config"
   <> short 'c'
   <> metavar "CONFIG"
   <> help "The location of the server configuration"
   <> showDefault
   <> value defaultConfig
     )

optsInfo :: Text -> ParserInfo Options
optsInfo defaultConfig = info (opts defaultConfig <**> helper)
  ( fullDesc
  <> progDesc "Launch the server"
  <> header "Raffle Assistant"
  )

loadConfig e = do
  let filePath =
        case e of
          Testing -> "test-config.json"
          Production -> "config.json"
  (Options filePath) <- execParser (optsInfo filePath)
  decode @Config . toS <$> readFile (toS filePath)
