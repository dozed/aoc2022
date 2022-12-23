{-# LANGUAGE QuasiQuotes #-}

module Day15 where

import Control.Monad (void)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (lstrip, regularParse)

testInput :: String
testInput = lstrip [r|
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
|]

type X = Int
type Y = Int
type Pos = (X, Y)
type SensorPos = Pos
type BeaconPos = Pos
data Info = Info SensorPos BeaconPos
            deriving (Eq, Show, Ord)

positiveNumberParser :: Parser Int
positiveNumberParser = read <$> many1 digit

negativeNumberParser :: Parser Int
negativeNumberParser = do
  void $ char '-'
  n <- positiveNumberParser
  return $ -n

numberParser :: Parser Int
numberParser = negativeNumberParser <|> positiveNumberParser

infoParser :: Parser Info
infoParser = do
  void $ string "Sensor at x="
  sx <- numberParser
  void $ string ", y="
  sy <- numberParser
  void $ string ": closest beacon is at x="
  bx <- numberParser
  void $ string ", y="
  by <- numberParser
  return $ Info (sx, sy) (bx, by)

infosParser :: Parser [Info]
infosParser = endBy1 infoParser endOfLine

getManhattanDistance :: Pos -> Pos -> Int
getManhattanDistance (x1, y1) (x2, y2) =
  let xn = abs (x1 - x2)
      yn = abs (y1 - y2)
      md = xn + yn
  in md

getCoveredRowPoints :: Pos -> Int -> [Pos]
getCoveredRowPoints (x, y) distance =
  let from = x - distance
      to = x + distance
      covered = [(t, y) | t <- [from..to]]
  in covered

getUpwardPoint :: Pos -> Int -> Pos
getUpwardPoint (x, y) distance = (x, y - distance)

getDownwardPoint :: Pos -> Int -> Pos
getDownwardPoint (x, y) distance = (x, y + distance)

day15 :: IO ()
day15 = do
  let input = testInput

  infos <- case regularParse infosParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  print infos

  -- compute covered positions for all Sensor/Beacon pairs
  -- compute covered positions for one Sensor/Beacon pair
  -- - build pyramid up
  --   - get covered position for one row
  --   - build covered row positions for all required rows
  -- - build pyramid down
