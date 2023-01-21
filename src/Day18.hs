{-# LANGUAGE QuasiQuotes #-}

module Day18 where

import Control.Monad (void)
import Data.List.HT (removeEach)
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ

import Util (count, regularParse, lstrip)

miniInput :: String
miniInput = lstrip [r|
1,1,1
2,1,1
|]

testInput :: String
testInput = lstrip [r|
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
|]

type X = Int
type Y = Int
type Z = Int

type Pos = (X, Y, Z)

positionParser :: Parser Pos
positionParser = do
  x <- int
  void $ char ','
  y <- int
  void $ char ','
  z <- int
  return (x, y, z)

positionsParser :: Parser [Pos]
positionsParser = endBy1 positionParser endOfLine

isAdjacent :: Pos -> Pos -> Bool
isAdjacent (x1, y1, z1) (x2, y2, z2) = abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2) <= 1

countFreeSides' :: Pos -> [Pos] -> Int
countFreeSides' cubePos cubePositions =
  let numAdjacents = count (isAdjacent cubePos) cubePositions
      numFreeSides = 6 - numAdjacents
  in numFreeSides

countFreeSides :: [Pos] -> Int
countFreeSides cubePositions =
  let cubes = removeEach cubePositions
      numFreeSides = sum . map (uncurry countFreeSides') $ cubes
  in numFreeSides

day18 :: IO ()
day18 = do
  -- let input = testInput
  input <- readFile "input/Day18.txt"

  cubes <- case regularParse positionsParser input of
    Left msg -> fail $ show msg
    Right xs -> pure xs

  print $ countFreeSides cubes
