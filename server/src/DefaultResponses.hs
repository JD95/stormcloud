{-# LANGUAGE OverloadedStrings #-}

module DefaultResponses
  ( errorJson
  ) where

import Data.Aeson hiding (json)
import Prelude ()
import Protolude
import Web.Spock

import ServerTypes

errorJson :: Int -> Text -> ApiAction context a
errorJson code message = json $ object ["code" .= code, "message" .= message]
