{-# LANGUAGE DataKinds #-}

module ServerTypes where

import           Control.Concurrent.STM.TVar
import           Data.HVect
import           Data.Int
import           Database.Persist.Postgresql (SqlBackend)
import           Protolude
import           Web.Spock
import           Web.Spock.Config

import           Action.Audit
import           Action.FileServer
import           Config
import           DatabaseTypes

newtype ServerSecret =
  ServerSecret ByteString

data ServerState
  = ServerState
  { config :: Config
  , secret :: ServerSecret
  , cmds   :: TVar CommandHistory
  }

instance FileServerConfig ServerState where
  fileServerIp = fileServerIp . config
  fileServerPort = fileServerPort . config

instance Audit ServerState where
  cmdHistory = cmds

type API context = SpockCtxM context SqlBackend (Maybe UserId) ServerState ()

type ApiAction context = SpockActionCtx context SqlBackend (Maybe UserId) ServerState

type LoggedOut = HVect '[]

