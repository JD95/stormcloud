{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Types
  ( Email
  , Name
  ) where

import Data.Aeson
import GHC.Generics
import Prelude ()
import Protolude

newtype Email =
  Email Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype Name =
  Name Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
