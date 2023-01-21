{-# LANGUAGE QuasiQuotes #-}

module Day18 where

import Control.Monad (void)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.List.HT (removeEach)
import Data.Set (Set)
import qualified Data.Set as S
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

getX :: Pos -> X
getX (x, _, _) = x

getY :: Pos -> Y
getY (_, y, _) = y

getZ :: Pos -> Z
getZ (_, _, z) = z

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

getNeighbours :: Pos -> Set Pos
getNeighbours (x, y, z) = S.fromList [(x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1)]

type BoundingArea = (X, X, Y, Y, Z, Z)

getBoundingArea :: [Pos] -> BoundingArea
getBoundingArea droplet = 
  let minX = getX . minimumBy (compare `on` getX) $ droplet
      maxX = getX . maximumBy (compare `on` getX) $ droplet
      minY = getY . minimumBy (compare `on` getY) $ droplet
      maxY = getY . maximumBy (compare `on` getY) $ droplet
      minZ = getZ . minimumBy (compare `on` getZ) $ droplet
      maxZ = getZ . maximumBy (compare `on` getZ) $ droplet
  in (minX - 1, maxX + 1, minY - 1, maxY + 1, minZ - 1, maxZ + 1)

day18 :: IO ()
day18 = do
  -- let input = testInput
  input <- readFile "input/Day18.txt"

  cubes <- case regularParse positionsParser input of
    Left msg -> fail $ show msg
    Right xs -> pure xs

  -- part 1
  print $ countFreeSides cubes

  -- part 2
  putStrLn "part 2"
