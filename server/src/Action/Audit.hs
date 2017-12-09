{-# LANGUAGE OverloadedStrings #-}

module Action.Audit where

import           Control.Concurrent.STM.TVar
import qualified Control.Monad               as M
import qualified Data.ByteString.Char8       as B
import           Data.Sequence               ((<|))
import qualified Data.Sequence               as S
import           Data.Time.Calendar
import           Data.Time.Clock
import           DatabaseTypes
import           GHC.Natural
import           Prelude                     (Show (..))
import           Protolude                   hiding (empty, guard, show,
                                              splitAt)
import           System.Random

import           Error

data StorageAction
  = Store
  | Retrieve
  | Delete
    deriving (Show)

class ToHeader a where
  toHeader :: a -> ByteString

instance ToHeader StorageAction where
  toHeader Store = "store"
  toHeader Retrieve = "retrieve"
  toHeader Delete = "delete"

intToStorageAction :: Int -> StorageAction
intToStorageAction i =
  case i `mod` 3 of
    0 -> Store
    1 -> Retrieve
    2 -> Delete

instance Random StorageAction where
  random g = first intToStorageAction (next g)
  randomR _ g = random g

data UserAction
  = Login
  | Logout
    deriving (Show)

data Action a = Action
  { actionTime :: UTCTime
  , actionUser :: User
  , action     :: a
  }

instance (ToHeader a) => ToHeader (Action a) where
  toHeader (Action _ _ a) = toHeader a

packStorageHistory :: Seq (Action StorageAction) -> ByteString
packStorageHistory = foldl' f ""
  where f :: ByteString -> Action StorageAction -> ByteString
        f b (Action _ _ Store)    = b <> "s"
        f b (Action _ _ Retrieve) = b <> "r"
        f b (Action _ _ Delete)   = b <> "d"

instance (Show a) => Show (Action a) where
  show (Action _ _ a) = show a

data CommandHistory = CommandHistory
  { storageActions :: Seq (Action StorageAction)
  , userActions    :: Seq (Action UserAction)
  } deriving (Show)

class Audit a where
  cmdHistory :: a -> TVar CommandHistory

randomStorageActions :: RandomGen g => g -> [Action StorageAction]
randomStorageActions g = take 50 $ (Action time user) <$> (randoms g :: [StorageAction])
  where time = UTCTime (fromGregorian 2017 12 1) (secondsToDiffTime 3000)
        user = User "jeff"

instance Random CommandHistory where
  random g = (CommandHistory (S.fromList $ randomStorageActions g) S.empty, g)
  randomR _ g = random g

initialCommandHistory :: CommandHistory
initialCommandHistory = CommandHistory S.empty S.empty

recordUserAction :: Action UserAction -> CommandHistory -> CommandHistory
recordUserAction a (CommandHistory ss us) = CommandHistory ss (a <| us)

recordStorageAction :: Action StorageAction -> CommandHistory -> CommandHistory
recordStorageAction a (CommandHistory ss us) = CommandHistory (a <| ss) us

popAction ::
     CommandHistory
  -> (CommandHistory, Seq (Action UserAction), Seq (Action StorageAction))
popAction (CommandHistory ss us) = (CommandHistory ss' us', dus, dss)
  where
    (ss', dss) = S.splitAt 50 ss
    (us', dus) = S.splitAt 50 us

verifyCommand :: Char -> Action StorageAction -> Validation ()
verifyCommand 's' (Action _ _ Store)    = pure ()
verifyCommand 'r' (Action _ _ Retrieve) = pure ()
verifyCommand 'd' (Action _ _ Delete)   = pure ()
verifyCommand c a = Left . ErrorMessage $ B.pack $ "History mismatch at " <> [c] <> " " <> show a

verifyCommandHistory :: ByteString -> CommandHistory -> Validation ()
verifyCommandHistory b c = do
  when (B.length b /= (S.length . storageActions) c) $
    (Left . ErrorMessage $ "Lengths of histories do not match")
  foldr (>>) (pure ()) $
    S.zipWith verifyCommand (S.fromList (B.unpack b)) (storageActions c)

