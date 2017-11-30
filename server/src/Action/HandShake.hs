{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Action.HandShake
  ( FileServerIp
  , FileServerPort
  , FileServerConfig(..)
  ) where

import Protolude
import           Control.Monad.Loops
import           Crypto.Saltine.Class
import           Crypto.Saltine.Core.SecretBox
import           Crypto.Saltine.Internal.ByteSizes
import           Data.Aeson                        hiding (decode, encode)
import           Data.ByteString
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Base16            as B16
import qualified Data.ByteString.Char8             as BC
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics
import           GHC.Natural
import           Network.Simple.TCP
import           Text.Read
import Control.Concurrent.STM
import qualified Data.Sequence as S
import Data.Sequence (Sequence)

import           Action.Encryption
import           Action.FileServer
import           Action.Parser

pushCommand :: Sequence  -> (Text, Char)
pushCommand t = 
