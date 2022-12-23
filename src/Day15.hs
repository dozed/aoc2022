{-# LANGUAGE QuasiQuotes #-}

module Day15 where

import Control.Monad (void)
import Data.Function (on)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.RawString.QQ

import Util (count, lstrip, regularParse)

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

getSensorPos :: Info -> Pos
getSensorPos (Info sp _) = sp

getBeaconPos :: Info -> Pos
getBeaconPos (Info _ bp) = bp

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

getCoveredRowPoints :: Pos -> Int -> Set Pos
getCoveredRowPoints (x, y) distance =
  let from = x - distance
      to = x + distance
      covered = S.fromList [(t, y) | t <- [from..to]]
  in covered

getUpwardPos :: Pos -> Pos
getUpwardPos (x, y) = (x, y - 1)

getDownwardPos :: Pos -> Pos
getDownwardPos (x, y) = (x, y + 1)

getCoveredPositions :: Pos -> Int -> Set Pos
getCoveredPositions midPos 0 = S.singleton midPos
getCoveredPositions midPos width =
  let rowPos = getCoveredRowPoints midPos width
      upwardMidPos = getUpwardPos midPos
      upwardCoveredPositions = getCoveredPositions upwardMidPos (width - 1)
      downwardMidPos = getDownwardPos midPos
      downwardCoveredPositions = getCoveredPositions downwardMidPos (width - 1)
      covered = S.unions [rowPos, upwardCoveredPositions, downwardCoveredPositions]
  in covered

showField :: Set Pos -> Set Pos -> Set Pos -> String
showField sensorPositions beaconPositions coveredPositions =
  let allPos = S.unions [sensorPositions, beaconPositions, coveredPositions]
      minX = fst . minimumBy (compare `on` fst) $ allPos
      maxX = fst . maximumBy (compare `on` fst) $ allPos
      minY = snd . minimumBy (compare `on` snd) $ allPos
      maxY = snd . maximumBy (compare `on` snd) $ allPos
      getPixel x y
        | S.member (x, y) sensorPositions = 'S'
        | S.member (x, y) beaconPositions = 'B'
        | S.member (x, y) coveredPositions = '#'
        | otherwise = '.'
      xxs = [[getPixel x y | x <- [minX..maxX]] | y <- [minY..maxY]]
      txt = intercalate "\n" xxs
  in txt

getNonBeaconPositionsInRow :: Int -> Set Pos -> Set Pos -> Int
getNonBeaconPositionsInRow y beaconPositions coveredPositions =
  let minX = fst . minimumBy (compare `on` fst) $ coveredPositions
      maxX = fst . maximumBy (compare `on` fst) $ coveredPositions
      coveredPositionsInRow = count (\x -> S.member (x, y) coveredPositions && (not . S.member (x, y)) beaconPositions) [minX..maxX]
  in coveredPositionsInRow

day15 :: IO ()
day15 = do
  let input = testInput

  infos <- case regularParse infosParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  print infos

  let sensorPositions = S.fromList $ map (\(Info sp _) -> sp) infos
      beaconPositions = S.fromList $ map (\(Info _ bp) -> bp) infos
      coveredPositions = S.unions $ map (\(Info sp bp) -> getCoveredPositions sp (getManhattanDistance sp bp)) infos

  print sensorPositions
  print beaconPositions
  print coveredPositions

  putStrLn $ showField sensorPositions beaconPositions S.empty
  putStrLn "---"
  putStrLn $ showField sensorPositions beaconPositions coveredPositions
  putStrLn "---"
  print $ getNonBeaconPositionsInRow 10 beaconPositions coveredPositions
