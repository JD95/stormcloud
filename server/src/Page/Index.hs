{-# LANGUAGE OverloadedStrings #-}
module Page.Index where

import           Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import           Crypto.Random
import           Data.Aeson hiding (json)
import           Data.HVect
import           Data.Time.Clock
import qualified Database.Persist as P
import           Database.Persist hiding (get)
import           Database.Persist.Postgresql hiding (get)
import           GHC.Generics
import           Lucid
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Static
import           Prelude ((==), (!!))
import           Protolude hiding (get)
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid

import           ServerTypes

index :: Text -> ApiAction context ()
index nameOfAppJs =
  lucid $ do
    head_ $ do
      meta_
        [ name_ "google-signin-client_id"
        , content_
            "942494335154-qltqhah21jbn3shv1a8pn79l0sd6s608.apps.googleusercontent.com"
        ]
      termWith
        "script"
        [src_ "https://apis.google.com/js/platform.js", async_ "defer"]
        ""
      termWith
        "script"
        [ src_
            "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
        ]
        ""
      termWith "script" [src_ "https://checkout.stripe.com/checkout.js"] ""
      link_
        [ rel_ "stylesheet"
        , href_
            "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        , integrity_
            "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u"
        , crossorigin_ "anonymous"
        ]
    body_ $ do
      termWith "script" [src_ "qrious.min.js"] ""
      termWith "script" [src_ nameOfAppJs, async_ "defer"] ""
