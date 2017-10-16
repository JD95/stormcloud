{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module DatabaseActions where

import Prelude (id)
import Protolude hiding (get, to, (&))
import Database.Persist
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Web.Spock hiding (get)
import Data.Maybe
import Data.Traversable
import Control.Monad
import Data.Time
import Control.Monad.IO.Class
import Data.HVect
import Control.Monad.Reader
import Control.Arrow
import Lens.Micro.Platform

import DatabaseTypes
import ServerTypes
import Utilities

getUserFromCookie :: ApiAction context (Maybe (Entity User))
getUserFromCookie = do
  token' <- cookie "auth"
  fmap (join . join) $ for token' $ \token -> do
    currTime <- liftIO getCurrentTime
    sess <- runSQL $ selectFirst [SessionToken ==. token, SessionExpration >. currTime] []
    for sess $ \(Entity _ (Session _ _ u)) ->
      runSQL . getEntity $ u

-- | Extracts values from returned entities and apply f
valueQuery q f filters selects = runSQL (q filters selects)
                             >>= traverse (\i -> pure $ i ^. to entityVal . f)

