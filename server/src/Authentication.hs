{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Authentication
  ( genToken
  , genSession
  , ServerSecret(..)
  , logout
  , login
  ) where

import Control.Monad.IO.Class
import Crypto.Hash.SHA256
import Crypto.Random
import Data.ByteArray
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HVect
import Data.List (lookup, replicate)
import Data.Maybe
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Units
import Data.Traversable
import qualified Database.Persist as P
import Database.Persist hiding (get)
import Network.Wai
import Prelude ()
import Protolude
import ServerTypes
import Web.Spock

import DatabaseTypes
import DefaultResponses
import Login
import Utilities

newtype ServerSecret =
  ServerSecret ByteString

sessionLength = 600 -- seconds

genToken ::
     ServerSecret -- ^ Random bytes unique to each bootup
  -> ApiAction LoggedOut (Maybe Text)
genToken (ServerSecret s) = do
  headers <- fmap requestHeaders request
  num <- liftIO (getRandomBytes 16 :: IO ByteString)
  time <- liftIO $ fmap (diffTimeToPicoseconds . utctDayTime) getCurrentTime
  let sessionId = do
        userAgent <- lookup "User-Agent" headers
        referer <- lookup "Referer" headers
        pure . B.concat $ [num, show time, userAgent, referer, s]
  for sessionId $ \sid -> pure (decodeUtf8 . encode . hash $ sid)

genSession ::
     Text -- ^ The google token string from client
  -> ServerSecret -- ^ Random bytes unique to each bootup
  -> ApiAction LoggedOut (Maybe ((Key User, User), Session))
genSession idToken s = do
  t <- verify idToken
  c' <- genToken s
  expr <- liftIO $ addUTCTime sessionLength <$> getCurrentTime
  fmap join . for c' $ \c ->
    for t $ \token -> do
      user' <- runSQL . getBy . UserEmailAddress $ email token
      (userId, user) <-
        case user' of
          Nothing
        -- If User does not exist, create one
           -> do
            let newUser = User (email token)
            i <- runSQL $ insert newUser
            pure (i, newUser)
          Just (Entity i u)
        -- Delete any existing sessions
           -> do
            runSQL $ deleteWhere [SessionUser ==. i]
            pure (i, u)
      pure ((userId, user), Session c expr userId)

login :: ServerSecret -> Text -> ApiAction LoggedOut ()
login serverSecret idToken = do
  session <- genSession idToken serverSecret
  case session of
    Nothing -> errorJson 2 "Login Failed"
    Just (userInfo, sess@(Session t e u)) -> do
      setCookie "auth" t defaultCookieSettings
      runSQL $ insert sess
      json userInfo

logout :: ListContains n (Entity User) xs => ApiAction (HVect xs) ()
logout = do
  token <- cookie "auth"
  flip (maybe (pure ())) token $ \t -> runSQL $ deleteWhere [SessionToken ==. t]
  writeSession Nothing
