{-# LANGUAGE QuasiQuotes #-}

module Day10 where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse)

testInput1 :: String
testInput1 = [r|noop
addx 3
addx -5
|]

data Op = AddX Int | Noop
          deriving (Eq, Show)

positiveNumberParser :: Parser Int
positiveNumberParser = read <$> many1 digit

negativeNumberParser :: Parser Int
negativeNumberParser = do
  void $ char '-'
  n <- positiveNumberParser
  return $ -n

numberParser :: Parser Int
numberParser = negativeNumberParser <|> positiveNumberParser

noopParser :: Parser Op
noopParser = Noop <$ try (string "noop")

addXParser :: Parser Op
addXParser = do
  void $ try (string "addx ")
  n <- numberParser
  return $ AddX n

opParser :: Parser Op
opParser = noopParser <|> addXParser

opsParser :: Parser [Op]
opsParser = endBy1 opParser endOfLine

day10 :: IO ()
day10 = do
  let input = testInput1

  ops <- case regularParse opsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  print ops
