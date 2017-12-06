{-# LANGUAGE OverloadedStrings #-}
module Tests where

import           Control.Concurrent.STM.TVar
import qualified Data.ByteString as B

import           Action.Audit
import           Action.FileServer
import           Config
import Control.Monad.STM
import           ServerTypes

testCommands :: IO ()
testCommands = do
  config <- loadConfig Production
  cmdHist <- newTVarIO initialCommandHistory
  flip (maybe (print "Couldn't Load Config")) config $ \c ->  do
    let s = ServerState c (ServerSecret "") cmdHist
    r <- B.readFile "send.jpg"
    let a = Action undefined undefined Store
    print =<< store s a "iamafile\r\nDEDEFFFFF000012312323231566646DEDE" 
    let b = Action undefined undefined Retrieve
    print =<< retrieve s b "iamafile"
    let c = Action undefined undefined Delete
    print =<< delete s c "iamafile"
--    print =<< emergency s "Someone set us up the bomb!"

-- testValidateRetrieve :: IO ()
-- testValidateRetrieve = do
--   let a = Action undefined undefined Retrieve
--   config <- loadConfig Production
--   cmdHist <- newTVarIO initialCommandHistory
--   flip (maybe (print "Couldn't Load Config")) config $ \c ->  do
--     let s = ServerState c (ServerSecret "") cmdHist
--     r <- B.readFile "send.jpg"
--     let a = Action undefined undefined Retrieve
--     let m = Msg "retrieve" "DEDEDE\r\nr"
--     atomically (validateRetrieve s "retrieve" a m)
--     pure ()

