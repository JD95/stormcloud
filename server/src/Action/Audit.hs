module Action.Audit where

import Prelude ()
import Data.Sequence
import Data.Time.Clock
import GHC.Natural

import DatabaseTypes

data StorageAction
  = Upload
  | Download
  | Delete

data UserAction
  = Login
  | Logout

data Action a = Action
  { actionTime :: UTCTime
  , actionUser :: User
  , action :: a
  }

data CommandHistory = CommandHistory
  { storageActions :: Seq (Action StorageAction)
  , userActions :: Seq (Action UserAction)
  }

initialCommandHistory :: CommandHistory
initialCommandHistory = CommandHistory empty empty

recordUserAction :: Action UserAction -> CommandHistory -> CommandHistory
recordUserAction a (CommandHistory ss us) =
  CommandHistory ss (a <| us)

recordStorageAction :: Action StorageAction -> CommandHistory -> CommandHistory
recordStorageAction a (CommandHistory ss us) =
  CommandHistory (a <| ss) us

popAction :: CommandHistory -> (CommandHistory, Seq (Action UserAction), Seq (Action StorageAction))
popAction (CommandHistory ss us) = (CommandHistory ss' us', dus, dss)
  where (ss', dss) = splitAt 50 ss
        (us', dus) = splitAt 50 us
