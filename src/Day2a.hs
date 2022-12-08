{-# LANGUAGE QuasiQuotes #-}

module Day2a (day2a) where

import Text.RawString.QQ
import Text.Parsec
import Text.Parsec.String

import Util (regularParse)

testInput1 :: String
testInput1 = [r|A Y
B X
C Z
|]

testInput2 :: IO String
testInput2 = readFile "input/Day2.txt"

data Shape = Rock | Paper | Scissors
  deriving Show

data Outcome = Loss | Win | Draw
  deriving Show

getOutcome :: Shape -> Shape -> Outcome
getOutcome Rock Rock = Draw
getOutcome Rock Paper = Loss
getOutcome Rock Scissors = Win
getOutcome Paper Rock = Win
getOutcome Paper Paper = Draw
getOutcome Paper Scissors = Loss
getOutcome Scissors Rock = Loss
getOutcome Scissors Paper = Win
getOutcome Scissors Scissors = Draw

getShapeScore :: Shape -> Int
getShapeScore Rock = 1
getShapeScore Paper = 2
getShapeScore Scissors = 3

getOutcomeScore :: Outcome -> Int
getOutcomeScore Loss = 0
getOutcomeScore Draw = 3
getOutcomeScore Win = 6

getScore :: Shape -> Outcome -> Int
getScore shape outcome = getShapeScore shape + getOutcomeScore outcome

type Situation = (Shape, Shape)

-- A command file contains 0 or more lines, each of which is terminated
-- by the end-of-line character (eol).
situationParser :: Parser [Situation]
situationParser = do
  result <- many line
  eof
  pure result

-- Each line contains 1 situation
line :: Parser Situation
line = do
  part1 <- firstPart
  spaces
  part2 <- secondPart
  _ <- char '\n'
  pure (part1, part2)

firstPart :: Parser Shape
firstPart = do
  c <- oneOf "ABC"
  case c of
    'A' -> pure Rock
    'B' -> pure Paper
    'C' -> pure Scissors
    _ -> parserFail $ "invalid part1: " <> [c]

secondPart :: Parser Shape
secondPart = do
  c <- oneOf "XYZ"
  case c of
    'X' -> pure Rock
    'Y' -> pure Paper
    'Z' -> pure Scissors
    _ -> parserFail $ "invalid part2: " <> [c]

day2a :: IO ()
day2a = do
  -- let txt = testInput1
  txt <- testInput2

  situations <- case regularParse situationParser txt of
    Right as -> pure as
    Left b -> fail $ show b

  let outcomes = map (\(other, own) -> (own, getOutcome own other)) situations
      scores = map (uncurry getScore) outcomes
      totalScore = sum scores

  print totalScore
