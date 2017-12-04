module Error where

import qualified Data.ByteString.Char8 as BC

type Validation a = Either ErrorMessage a

newtype ErrorMessage =
  ErrorMessage BC.ByteString
  deriving (Show, Eq, Ord)

