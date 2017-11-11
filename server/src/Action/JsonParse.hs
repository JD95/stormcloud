{-# LANGUAGE OverloadedStrings, DeriveFunctor, TypeApplications #-}

module Action.JsonParse where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Text.Read
import Data.Tuple
import Control.Monad.Loops
import Control.Applicative
import GHC.Natural

newtype Parser a =
  Parser { runParser :: [ByteString] -> Either String ([ByteString], a) }
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (input, a)
  Parser f' <*> Parser x' =
    Parser $ \input ->
      let parseInput (rest, f) = fmap (fmap f) (x' rest)
      in either Left parseInput (f' input)

instance Alternative Parser where
  empty = Parser (const $ Left "")
  f <|> g = Parser $ \input ->
    either (const $ runParser g input) Right (runParser f input) 

instance Monad Parser where
  (Parser ma) >>= f = Parser $ \input ->
    case ma input of
      Right (rest, a) -> runParser (f a) rest
      Left e -> Left e
      
digit = "0123456789"

readByteString :: Read a => ByteString -> Either String a
readByteString = readEither . BC.unpack

parseInt :: [ByteString] -> Either String ([ByteString], Natural)
parseInt (input:more) = fmap ((,) more) . readByteString $ input

parseSymbol ::
     ByteString -> [ByteString] -> Either String ([ByteString], ByteString)
parseSymbol target (input:rest) =
  if target == input
    then Right (rest, target)
    else Left $ "Could not parse " ++ BC.unpack target

parseString :: [ByteString] -> Either String ([ByteString], ByteString)
parseString ("\"":rest) =
  case span ("\"" /=) rest of
    (_, []) -> Left "Unbalanced double quotes"
    (inner, "\"":more) -> Right (more, B.concat . intersperse " " $ inner)
parseString _ = Left "Could not parse opening string quote"

int = Parser parseInt

string = Parser parseString

quote = Parser (parseSymbol "\"")

colon = Parser (parseSymbol ":")

comma = Parser (parseSymbol ",")

openBracket = Parser (parseSymbol "[")
closeBracket = Parser (parseSymbol "]")

openCurly = Parser (parseSymbol "{")
closedCurly = Parser (parseSymbol "}")

symbol s = Parser (parseSymbol s)

parseUntil :: Parser many -> Parser end -> Parser [many]
parseUntil m e = Parser $ \input ->
  either (const $ runParser ((:) <$> m <*> parseUntil m e) input) (Right . fmap (const [])) (runParser e input)

between :: Parser start -> Parser end -> Parser a -> Parser [a]
between st end middle =
  st *> parseUntil middle end

seperators :: ByteString
seperators = ":,[]{}\""

breakOn :: (Char -> Bool) -> ByteString -> [ByteString]
breakOn pred = unfoldr f
  where f "" = Nothing
        f input | pred . BC.head $ input = Just $ BC.splitAt 1 input
                | otherwise = Just . BC.break pred $ input

parse (Parser f) =
  f . join . fmap (breakOn (`BC.elem` seperators)) . BC.words
