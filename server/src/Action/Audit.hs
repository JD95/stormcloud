{-# LANGUAGE OverloadedStrings #-}

module Action.Audit where

import qualified Control.Monad         as M
import qualified Data.ByteString.Char8 as B
import           Data.Sequence         ((<|))
import qualified Data.Sequence         as S
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

data Action a = Action
  { actionTime :: UTCTime
  , actionUser :: User
  , action     :: a
  }

instance (Show a) => Show (Action a) where
  show (Action _ _ a) = show a

data CommandHistory = CommandHistory
  { storageActions :: Seq (Action StorageAction)
  , userActions    :: Seq (Action UserAction)
  }

randomStorageActions :: RandomGen g => g -> [Action StorageAction]
randomStorageActions g = take 10 $ (Action time user) <$> (randoms g :: [StorageAction])
  where time = undefined
        user = undefined

instance Random CommandHistory where
  random g = undefined --next g

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
verifyCommand c (Action _ _ s) = undefined

verifyCommandHistory :: ByteString -> CommandHistory -> Either B.ByteString ()
verifyCommandHistory b c = do
  when (B.length b /= (S.length . storageActions) c) $
    (Left "Lengths of histories do not match")
  foldr (>>) (pure ()) $
    S.zipWith verifyCommand (S.fromList (B.unpack b)) (storageActions c)
