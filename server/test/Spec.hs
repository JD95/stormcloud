{-# LANGUAGE OverloadedStrings #-}

import Prelude ()
import Protolude hiding (get)
import Control.Concurrent
import Test.Hspec
import Network.Wreq
import Data.Aeson.Lens
import Lens.Micro.Platform

import App (launchServer)
import Config

main :: IO ()
main = do
  config <- loadConfig Testing
  maybe (print "Could not open config file") spec config

spec config = do
  print "Running Tests"
  forkIO $ do
    launchServer config
  threadDelay 5000
  hspec $ do
    describe "Main Application" $ do
      it "launches properly" $ do
        r <- get "http://localhost:4000/"
        r ^. responseStatus . statusCode `shouldBe` 200      
  print "Tests are completed!"
