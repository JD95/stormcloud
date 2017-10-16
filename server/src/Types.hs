{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Types (Email, Name) where

import Prelude ()
import Protolude
import Data.Aeson
import GHC.Generics

newtype Email = Email Text deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
newtype Name = Name Text deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
