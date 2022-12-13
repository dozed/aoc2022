{-# LANGUAGE QuasiQuotes #-}

module Day11 where

import Control.Monad (forM_)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse)

data Operation = MulWith Int
               | MulWithOld
               | AddWith Int
               deriving Show

data Monkey = Monkey {
  idx :: Int,
  startingItems :: [Int],
  operation :: Operation,
  testDivisor :: Int,
  trueThrowTo :: Int,
  falseThrowTo :: Int
} deriving Show

numberListParser :: Parser [Int]
numberListParser = sepBy1 (read <$> many1 digit) (string ", ")

operationParser :: Parser Operation
operationParser =
      try (MulWithOld <$ (char '*' >> char ' ' >> string "old"))
  <|> (char '*' >> char ' ' *> (MulWith . read <$> many1 digit))
  <|> (char '+' >> char ' ' *> (AddWith . read <$> many1 digit))

monkeyParser :: Parser Monkey
monkeyParser = do
  idx <- read <$> (string "Monkey " *> many1 digit <* string ":\n")
  startingItems <- string "  Starting items: " *> numberListParser <* char '\n'
  operation <- string "  Operation: new = old " *> operationParser <* char '\n'
  testDivisor <- string "  Test: divisible by " *> (read <$> many1 digit) <* char '\n'
  trueThrowTo <- string "    If true: throw to monkey " *> (read <$> many1 digit) <* char '\n'
  falseThrowTo <- string "    If false: throw to monkey " *> (read <$> many1 digit) <* char '\n'
  return Monkey {
    idx = idx,
    startingItems = startingItems,
    operation = operation,
    testDivisor = testDivisor,
    trueThrowTo = trueThrowTo,
    falseThrowTo = falseThrowTo
  }

monkeysParser :: Parser [Monkey]
monkeysParser = sepBy1 monkeyParser (char '\n')

testInput1 :: String
testInput1 = [r|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
|]

day11 :: IO ()
day11 = do
  let input = testInput1

  monkeys <- case regularParse monkeysParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  forM_ monkeys $ \monkey ->
    print monkey
