{-# LANGUAGE OverloadedStrings, TypeApplications, DeriveGeneric,
  DeriveAnyClass, FlexibleInstances #-}

module Login
  ( Email
  , GoogleToken(..)
  , verify
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Monoid
import GHC.Generics
import Lens.Micro.Platform ((.~), (^.))
import Network.Wreq
import Prelude ()
import Protolude

import Types

data GoogleToken = GoogleToken
  { iss :: Text -- ^ Either accounts.google.com or https://accounts.google.com
  , sub :: Text
  , azp :: Text
  , aud :: Text
  , iat :: Text
  , exp :: Text -- ^ Token Expiration
  , email :: Text
  , email_verified :: Text
  , name :: Text
  , picture :: Text
  } deriving (Show, Generic, ToJSON)

instance FromJSON GoogleToken where
  parseJSON =
    withObject "id_token" $ \o ->
      GoogleToken <$> o .: "iss" <*> o .: "sub" <*> o .: "azp" <*> o .: "aud" <*>
      o .: "iat" <*>
      o .: "exp" <*>
      o .: "email" <*>
      o .: "email_verified" <*>
      o .: "name" <*>
      o .: "picture"

verify :: (MonadIO m) => Text -> m (Maybe GoogleToken)
verify token = do
  let opts = defaults & param "id_token" .~ [token]
  bs <-
    liftIO $
    postWith
      opts
      "https://www.googleapis.com/oauth2/v3/tokeninfo"
      (toJSON (5 :: Int))
  liftIO $ print $ bs ^. responseBody
  pure (decode @GoogleToken (bs ^. responseBody))
