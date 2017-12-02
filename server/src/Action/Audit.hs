{-# LANGUAGE OverloadedStrings #-}

module Action.Audit where

import qualified Control.Monad         as M
import qualified Data.ByteString.Char8 as B
import           Data.Sequence         ((<|))
import qualified Data.Sequence         as S
import           Data.Time.Calendar
import           Data.Time.Clock
import           DatabaseTypes
import           GHC.Natural
import           Prelude               (Show (..))
import           Protolude             hiding (empty, guard, show, splitAt)
import           System.Random

data StorageAction
  = Store
  | Retrieve
  | Delete
    deriving (Show)

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

verifyCommand :: Char -> Action StorageAction -> Either B.ByteString ()
verifyCommand 's' (Action _ _ Store)    = pure ()
verifyCommand 'r' (Action _ _ Retrieve) = pure ()
verifyCommand 'd' (Action _ _ Delete)   = pure ()
verifyCommand c a = Left $ B.pack $ "History mismatch at " <> [c] <> " " <> show a

verifyCommandHistory :: ByteString -> CommandHistory -> Either B.ByteString ()
verifyCommandHistory b c = do
  when (B.length b /= (S.length . storageActions) c) $
    (Left "Lengths of histories do not match")
  foldr (>>) (pure ()) $
    S.zipWith verifyCommand (S.fromList (B.unpack b)) (storageActions c)

testVerifyCommandHistory :: IO ()
testVerifyCommandHistory = do
  g <- newStdGen
  let c@ (CommandHistory s _) = fst $ random g
  print $ verifyCommandHistory (packStorageHistory s) c
