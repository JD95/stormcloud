module Main where

import           Prelude   ()
import           Protolude

import           App       (launchServer)
import           Config

main :: IO ()
main = do
  config <- loadConfig Testing
  maybe (print "Failed to open config file") launchServer config
