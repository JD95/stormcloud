{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module DatabaseActions where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.HVect
import           Data.Maybe
import           Data.Time
import           Data.Traversable
import           Database.Persist
import           Database.Persist.Sql.Types.Internal (SqlBackend)
import           Lens.Micro.Platform
import           Prelude                             (id)
import           Protolude                           hiding (get, to, (&))
import           Web.Spock                           hiding (get)

import           DatabaseTypes
import           ServerTypes
import           Utilities

-- | An interface to the database
class (Monad m) => DataBase m where
  getActiveSession :: Text -> UTCTime -> m (Maybe (Entity Session))
  getUser :: Key User -> m (Maybe (Entity User))

instance (Monad m, HasSpock m, SpockConn m ~ SqlBackend) => DataBase m where
  getActiveSession token currTime = runSQL $
      selectFirst [SessionToken ==. token, SessionExpration >. currTime] []
  getUser = runSQL . getEntity

class (Monad m, DataBase m) => Request m where
  getCookie :: Text -> m (Maybe Text)

instance Request (ApiAction context) where
  getCookie = cookie

-- | Attempts to retrieve a user from the database using the
-- request cookie.
getUserFromCookie :: (MonadIO m, Request m) => m (Maybe (Entity User))
getUserFromCookie = do
  token' <- getCookie "auth"
  fmap (join . join) $ for token' $ \token -> do
    currTime <- liftIO getCurrentTime
    sess <- getActiveSession token currTime
    for sess $ \(Entity _ (Session _ _ u)) -> getUser u

-- | Extracts values from returned entities and apply f
valueQuery q f filters selects =
  runSQL (q filters selects) >>= traverse (\i -> pure $ i ^. to entityVal . f)
