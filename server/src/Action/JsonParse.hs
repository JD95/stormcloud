{-# LANGUAGE OverloadedStrings, DeriveFunctor, TypeApplications #-}

module Action.JsonParse where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Text.Read
import Data.Tuple

newtype Parser a =
  Parser ([ByteString] -> Either String ([ByteString], a))
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (input, a)
  Parser f' <*> Parser x' =
    Parser $ \input ->
      let parseInput (rest, f) = either Left (Right . fmap f) (x' rest)
      in either Left parseInput (f' input)

digit = "0123456789"

readByteString :: Read a => ByteString -> Either String a
readByteString = readEither . BC.unpack

parseInt :: [ByteString] -> Either String ([ByteString], Int)
parseInt (input:more) = fmap ((,) more) . readByteString $ input

parseSymbol ::
     ByteString -> [ByteString] -> Either String ([ByteString], ByteString)
parseSymbol target (input:rest) =
  if target == input
    then Right (rest, target)
    else Left $ "Could not parse " ++ BC.unpack target

parseString :: [ByteString] -> Either String ([ByteString], ByteString)
parseString ("\"":rest) =
  case span ((/=) "\"") rest of
    (_, []) -> Left "Unbalanced double quotes"
    (inner, "\"":more) -> Right (more, B.concat . intersperse " " $ inner)
parseString _ = Left "Could not parse opening string quote"

int = Parser parseInt

string = Parser parseString

quote = Parser (parseSymbol "\"")

colon = Parser (parseSymbol ":")

comma = Parser (parseSymbol ",")

seperators :: ByteString
seperators = ":,[]{}\""

breakOn :: (Char -> Bool) -> ByteString -> [ByteString]
breakOn pred = unfoldr f
  where f "" = Nothing
        f input | pred . BC.head $ input = Just $ BC.splitAt 1 input
                | otherwise = Just . BC.break pred $ input

parse (Parser f) =
  f . join . fmap (breakOn (`BC.elem` seperators)) . BC.words
