{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Utilities where

import           Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import qualified Database.Persist as P
import           Database.Persist hiding (get)
import           Database.Persist.Postgresql hiding (get)
import           Web.Spock

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend)
       => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn       
