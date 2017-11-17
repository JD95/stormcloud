{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Base16
  ( Base16
  , Base16Conv(..)
  , toBase16
  ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as BC
import           Data.Char

newtype Base16 a =
  Base16 a
  deriving (Functor)

class Base16Conv a where
  
  -- | Test values to see if they are already in Base16
  isBase16 :: a -> Bool

  -- | Convert a value into base16 and wrap it 
  encodeBase16 :: a -> Base16 a

  -- | Convert a value from base16
  fromBase16 :: Base16 a -> a

instance (Monoid m) => Monoid (Base16 m) where
  mempty = Base16 mempty
  mappend (Base16 a) (Base16 b) = Base16 (a `mappend` b)

instance Base16Conv ByteString where
  fromBase16 (Base16 b) = fst . B16.decode . BC.map toLower $ b
  encodeBase16 = Base16 . BC.map toUpper . B16.encode
  isBase16 = BC.all (`BC.elem` "ABCDEF0123456789")

{- |
  Encodes a value into base16 if it is not
  already, simply wraps the value otherwise
-}
toBase16 :: Base16Conv a => a -> Base16 a
toBase16 b
  | isBase16 b = Base16 b
  | otherwise = encodeBase16 b
