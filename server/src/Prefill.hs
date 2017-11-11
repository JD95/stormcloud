{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Prefill where

import Control.Monad.IO.Class
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader
import Database.Persist
import Database.Persist.Class
import Database.Persist.Postgresql
import Database.Persist.Sql.Types.Internal
import System.IO hiding (print)
import System.Process

import DatabaseTypes

createTestDb :: MonadIO m => m ()
createTestDb = do
  let p = (proc "psql" ["--username=postgres"]) {create_new_console = True}
  liftIO $
    readCreateProcess
      p
      "DROP DATABASE IF EXISTS foobar;\nCREATE DATABASE foobar;\n\\q\n"
  pure ()

loadTestData :: MonadIO m => ReaderT SqlBackend m ()
loadTestData = do
  user <- insert $ User "jeffreydwyer95@gmail.com"
  pure ()
