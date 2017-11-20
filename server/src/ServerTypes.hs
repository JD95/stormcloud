{-# LANGUAGE DataKinds #-}

module ServerTypes where

import           Data.HVect
import           Data.Int
import           Database.Persist.Postgresql (SqlBackend)
import           Web.Spock
import           Web.Spock.Config

import           DatabaseTypes

type API context = SpockCtxM context SqlBackend (Maybe UserId) () ()

type ApiAction context = SpockActionCtx context SqlBackend (Maybe UserId) ()

type LoggedOut = HVect '[]
