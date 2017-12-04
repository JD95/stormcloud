module Action.File where

import           Control.Concurrent.STM
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
import qualified Data.Sequence                     as S
import           GHC.Generics
import           GHC.Natural
import           Network.Simple.TCP
import           Prelude                           ()
import           Protolude
import           Text.Read

import           Action.Audit
import           Action.Encryption
import           Action.FileServer
import           ServerTypes


sendEmergencyMessage = undefined

