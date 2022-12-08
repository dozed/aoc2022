{-# LANGUAGE QuasiQuotes #-}

module Day4 where

import Control.Monad (forM_, void)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse)

data Range a = Range a a
  deriving Show

rangeContains :: Ord a => Range a -> Range a -> Bool
rangeContains (Range a1 a2) (Range b1 b2) = a1 <= b1 && b2 <= a2

rangeContains' :: Ord a => Range a -> Range a -> Bool
rangeContains' r1 r2 = rangeContains r1 r2 || rangeContains r2 r1

rangeOverlaps :: Ord a => Range a -> Range a -> Bool
rangeOverlaps (Range a1 a2) (Range b1 b2) =
     (b2 >= a1 && b2 <= a2)
  || (a2 >= b1 && a2 <= b2)

testInput1 :: String
testInput1 = [r|
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
|]

testInput2 :: IO String
testInput2 = readFile "input/Day4.txt"

parseInput :: Parser [(Range Int, Range Int)]
parseInput = do
  optional endOfLine
  result <- many parseLine
  eof
  pure result

parseLine :: Parser (Range Int, Range Int)
parseLine = do
  a <- read <$> many1 digit
  void $ char '-'
  b <- read <$> many1 digit
  void $ char ','
  c <- read <$> many1 digit
  void $ char '-'
  d <- read <$> many1 digit
  void endOfLine
  pure (Range a b, Range c d)

day4 :: IO ()
day4 = do
  -- let txt = testInput1
  txt <- testInput2

  assignments <- case regularParse parseInput txt of
    Left e -> fail $ show e
    Right rs -> pure rs

  -- debug
  forM_ assignments print

  -- part 1
  let totalContains = length . filter (uncurry rangeContains') $ assignments
  print totalContains

  -- part 2
  let totalOverlaps = length . filter (uncurry rangeOverlaps) $ assignments
  print totalOverlaps

