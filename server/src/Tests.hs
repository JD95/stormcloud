{-# LANGUAGE OverloadedStrings #-}
module Tests where

import           Control.Concurrent.STM.TVar

import           Action.Audit
import           Action.FileServer
import           Config
import           ServerTypes

testCommands :: IO ()
testCommands = do
  config <- loadConfig Testing
  cmdHist <- newTVarIO initialCommandHistory
  flip (maybe (print "Couldn't Load Config")) config $ \c ->  do
    let s = ServerState c (ServerSecret "") cmdHist
    print =<< store s "DEDEDEDEDE"
    print =<< delete s "DEDEDEDEDE"
    print =<< emergency s "Someone set us up the bomb!"
