{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric, TypeApplications, OverloadedStrings,
  EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs,
  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, QuasiQuotes, TemplateHaskell,
  TypeFamilies #-}

module DatabaseTypes where

import Data.Aeson hiding (json)
import Data.Time.Clock
import qualified Database.Persist as P
import Database.Persist hiding (get)
import Database.Persist.Postgresql hiding (get)
import Database.Persist.TH
import Prelude ()
import Protolude hiding (get)

share
  [mkPersist sqlSettings {mpsGenerateLenses = True}, mkMigrate "migrateAll"]
  [persistLowerCase|
    User json
      email Text
      UserEmailAddress email
      deriving (Show)

    Session
      token Text
      expration UTCTime
      user UserId
      Sid token user
      deriving (Show)

|]
