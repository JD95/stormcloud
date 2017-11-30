{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger          (LoggingT, runStdoutLoggingT)
import           Crypto.Random
import           Data.Aeson                    hiding (json)
import           Data.HVect
import qualified Data.List                     as List
import           Data.Time.Clock
import           Database.Persist              hiding (get)
import qualified Database.Persist              as P
import           Database.Persist.Postgresql   hiding (get)
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Static
import           Prelude                       ((!!), (==))
import           Protolude                     hiding (get, getContents)
import           System.IO                     hiding (print, putStrLn)
import           System.Process
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid

import           Action.UploadImage
import           Authentication
import           Config
import           DatabaseActions
import           DatabaseTypes
import           DefaultResponses
import           Login
import           Prefill
import           ServerTypes
import           Utilities

-- | A unique bytestring for use in session id creation,
-- unique to each boot up
serverSecret :: IO ByteString
serverSecret = getRandomBytes 16

launchServer :: Config -> IO ()
launchServer config
 = do
  createTestDb
  let conn = toS (dbConnStr config) :: Text
  -- Connect to Postgresql database with a connection pool of 5
  pool <- runStdoutLoggingT $ createPostgresqlPool (toS conn) 5

  s <- serverSecret
  hist <- newTVarIO ""
  let st = ServerState config (ServerSecret s) hist

  spockCfg <- defaultSpockCfg Nothing (PCPool pool) st
  runStdoutLoggingT $
    (`runSqlPool` pool) $ do
      runMigration migrateAll
      loadTestData

  -- Initialize the application
  let middle = spock spockCfg app
  httpServer middle

httpServer = runSpock 4000

httpsServer mware
    -- Initialize the application
 = do
  app <- spockAsApp mware
  let serverSettings =
        defaultSettings & setPort 4000 & setServerName "Raffle App"
    -- Run the server using HTTPS and settings
  runTLS defaultTlsSettings serverSettings app

-- | Default privilages
initHook :: ApiAction () LoggedOut
initHook = pure HNil

-- | When user has an active session
authHook :: ApiAction (HVect xs) (HVect ((Entity User) ': xs))
authHook = do
  oldContext <- getContext
  user' <- getUserFromCookie
  maybe
    (errorJson 0 "User is not logged in!")
    (\u -> pure (u :&: oldContext))
    user'

-- | A helper for actions that make use of the user session
withUser ::
     ListContains n (Entity User) xs
  => (Entity User -> a) -- ^ Accessor to the Entity User
  -> (a -> ApiAction (HVect xs) b) -- ^ Api action using part of entity
  -> ApiAction (HVect xs) b
withUser from f = do
  user <- fmap findFirst getContext
  f (from user)

-- | The routing for the application
app :: API ()
app =
  prehook initHook $ do
    -- User login
    get ("login" <//> var) $ login
    prehook authHook $ do
      post "logout" logout
      post "upload-image" uploadimage
