{-# LANGUAGE QuasiQuotes #-}

module Day15 where

import Control.Monad (forM_, void, when)
import Data.Function (on)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.Maybe (fromMaybe, isNothing)
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
type ManhattanDistance = Int
data Info = Info SensorPos BeaconPos ManhattanDistance
            deriving (Eq, Show, Ord)

getSensorPos :: Info -> Pos
getSensorPos (Info sp _ _) = sp

getBeaconPos :: Info -> Pos
getBeaconPos (Info _ bp _) = bp

mkSensorInfo :: Pos -> Pos -> Info
mkSensorInfo sensorPos beaconPos =
  let dist = getManhattanDistance sensorPos beaconPos
  in Info sensorPos beaconPos dist

positiveNumberParser :: Parser Int
positiveNumberParser = read <$> many1 digit

negativeNumberParser :: Parser Int
negativeNumberParser = do
  void $ char '-'
  n <- positiveNumberParser
  return (-n)

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
  let sensorPos = (sx, sy)
      beaconPos = (bx, by)
      sensorInfo = mkSensorInfo sensorPos beaconPos
  return sensorInfo

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

isCoveredBySensor :: Info -> Pos -> Bool
isCoveredBySensor (Info (sx, sy) _ d) (x, y) =
  let dx = abs (sx - x)
      dy = abs (sy - y)
      covered = dy <= d - dx
  in covered

isCoveredBySensor' :: Info -> Pos -> Bool
isCoveredBySensor' (Info sp _ d) pos =
  let d' = getManhattanDistance sp pos
      covered = d' <= d
  in covered

countNonBeaconPositionsInRow :: Int -> Set Pos -> Set Pos -> Int
countNonBeaconPositionsInRow y beaconPositions coveredPositions =
  let minX = fst . minimumBy (compare `on` fst) $ coveredPositions
      maxX = fst . maximumBy (compare `on` fst) $ coveredPositions
      coveredPositionsInRow = count (\x -> S.member (x, y) coveredPositions && (not . S.member (x, y)) beaconPositions) [minX..maxX]
  in coveredPositionsInRow

getMinXByInfo :: Info -> X
getMinXByInfo (Info (sx, _) _ d) = sx - d

getMaxXByInfo :: Info -> X
getMaxXByInfo (Info (sx, _) _ d) = sx + d

getMinYByInfo :: Info -> Y
getMinYByInfo (Info (_, sy) _ d) = sy - d

getMaxYByInfo :: Info -> Y
getMaxYByInfo (Info (_, sy) _ d) = sy + d

getMinXByInfos :: [Info] -> X
getMinXByInfos infos = minimum $ map getMinXByInfo infos

getMaxXByInfos :: [Info] -> X
getMaxXByInfos infos = maximum $ map getMaxXByInfo infos

getMinYByInfos :: [Info] -> X
getMinYByInfos infos = minimum $ map getMinYByInfo infos

getMaxYByInfos :: [Info] -> X
getMaxYByInfos infos = maximum $ map getMaxYByInfo infos

countNonBeaconPositionsInRow' :: Int -> [Info] -> Set Pos -> Int
countNonBeaconPositionsInRow' y infos beaconPositions =
  let minX = getMinXByInfos infos
      maxX = getMaxXByInfos infos
      coveredPositionsInRow = count (\x -> any (\i -> isCoveredBySensor' i (x, y)) infos && (not . S.member (x, y)) beaconPositions) [minX..maxX]
  in coveredPositionsInRow

getTuningSignal :: Pos -> Integer
getTuningSignal (x, y) = fromIntegral x * 4000000 + fromIntegral y

getSkippableX :: Info -> Pos -> Maybe Int
getSkippableX info@(Info (sx, sy) _ d) pos@(x, y)
  | not (isCoveredBySensor' info pos) = Nothing
  | otherwise = Just $ d - (x - sx) - abs(y - sy) + 1

getMaximumSkippableX :: [Info] -> Pos -> Maybe Int
getMaximumSkippableX [] _ = Nothing
getMaximumSkippableX (i:is) pos =
  case getSkippableX i pos of
    Nothing -> getMaximumSkippableX is pos
    Just skippable ->  case getMaximumSkippableX is pos of
      Nothing -> Just skippable
      Just skippable' -> if skippable > skippable' then Just skippable else Just skippable'

findUncoveredPos :: Int -> [Info] -> Pos -> Maybe Pos
findUncoveredPos fieldSize infos pos@(x, y) =
  let skippableXPositionsMaybe = getMaximumSkippableX infos pos
      isUncovered = isNothing skippableXPositionsMaybe
  in
    if isUncovered then Just pos
    else
      let skippableXPositions = fromMaybe 1 skippableXPositionsMaybe
          pos'@(x', y') = if x + skippableXPositions > fieldSize then (0, y + 1)
                          else (x + skippableXPositions, y)
      in if y' > fieldSize then Nothing
         else findUncoveredPos fieldSize infos pos'

day15 :: IO ()
day15 = do
  -- let input = testInput
  input <- readFile "input/Day15.txt"

  infos <- case regularParse infosParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  print infos

  let minX = getMinXByInfos infos
      maxX = getMaxXByInfos infos
      minY = getMinYByInfos infos
      maxY = getMaxYByInfos infos
      dx = abs (maxX - minX)
      dy = abs (maxY - minY)

  putStrLn $ "minX: " <> show minX <> " maxX: " <> show maxX <> " minY: " <> show minY <> " maxY: " <> show maxY
  putStrLn $ "dx: " <> show dx <> " dy: " <> show dy
  putStrLn $ "fieldSize: " <> show (dx * dy)
  -- minX: -1892089 maxX: 5929991 minY: -1916483 maxY: 5272410
  -- dx: 7822080 dy: 7188893
  -- fieldSize: 56232096157440

  -- part 1
  let sensorPositions = S.fromList $ map (\(Info sp _ _) -> sp) infos
      beaconPositions = S.fromList $ map (\(Info _ bp _) -> bp) infos

  print sensorPositions
  print beaconPositions
  -- print $ length coveredPositions

  -- putStrLn $ showField sensorPositions beaconPositions S.empty
  -- putStrLn "---"
  -- putStrLn $ showField sensorPositions beaconPositions coveredPositions
  -- putStrLn "---"
  -- print $ countNonBeaconPositionsInRow 10 beaconPositions coveredPositions

  -- print $ countNonBeaconPositionsInRow' 10 infos beaconPositions
  print $ countNonBeaconPositionsInRow' 2000000 infos beaconPositions

  -- part 2
  -- let uncoveredPos = findUncoveredPos 20 infos (0, 0)
  let uncoveredPos = findUncoveredPos 4000000 infos (0, 0)

  case uncoveredPos of
    Nothing -> putStrLn "No uncovered pos found"
    Just pos -> putStrLn $ "uncovered position: " <> show pos <> " tuning signal: " <> show (getTuningSignal pos)
