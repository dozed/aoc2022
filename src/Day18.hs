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

import Util (count, filterNot, lstrip, regularParse)

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

type Droplet = Set Pos

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

getNeighbours :: Pos -> [Pos]
getNeighbours (x, y, z) = [(x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1)]

isAdjacent :: Pos -> Pos -> Bool
isAdjacent (x1, y1, z1) (x2, y2, z2) = abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2) <= 1

countAdjacentPositions :: Set Pos -> Pos -> Int
countAdjacentPositions positions position = S.size . S.filter (isAdjacent position) $ positions

countFreeSides' :: Pos -> [Pos] -> Int
countFreeSides' cubePos cubePositions =
  let numAdjacents = count (isAdjacent cubePos) cubePositions
      numFreeSides = 6 - numAdjacents
  in numFreeSides

countFreeSides :: Droplet -> Int
countFreeSides cubePositions =
  let cubes = removeEach $ S.toList cubePositions
      numFreeSides = sum . map (uncurry countFreeSides') $ cubes
  in numFreeSides

type BoundingArea = (X, X, Y, Y, Z, Z)

getBoundingArea :: Droplet -> BoundingArea
getBoundingArea droplet =
  let minX = getX . minimumBy (compare `on` getX) $ droplet
      maxX = getX . maximumBy (compare `on` getX) $ droplet
      minY = getY . minimumBy (compare `on` getY) $ droplet
      maxY = getY . maximumBy (compare `on` getY) $ droplet
      minZ = getZ . minimumBy (compare `on` getZ) $ droplet
      maxZ = getZ . maximumBy (compare `on` getZ) $ droplet
  in (minX - 1, maxX + 1, minY - 1, maxY + 1, minZ - 1, maxZ + 1)

isInsideBoundingArea :: BoundingArea -> Pos -> Bool
isInsideBoundingArea (minX, maxX, minY, maxY, minZ, maxZ) (x, y, z) =
  minX <= x && x <= maxX && minY <= y && y <= maxY && minZ <= z && z <= maxZ

getStartPos :: BoundingArea -> Pos
getStartPos (minX, _, minY, _, minZ, _) = (minX, minY, minZ)

type ToVisit = [Pos]
type Visited = Set Pos
type Reachables = Set Pos

getReachables' :: BoundingArea -> Droplet -> ToVisit -> Visited -> Reachables
-- getReachables' _ boundingArea droplet toVisit visited | trace (show toVisit <> " - " <> show visited) False = undefined
getReachables' _ _ [] visited = visited
getReachables' boundingArea droplet (current:nextToVisit) visited =
  let visited' = S.insert current visited
      neighbours = filter (isInsideBoundingArea boundingArea) $ getNeighbours current
      neighbours' = filterNot (`S.member` visited') neighbours
      neighbours'' = filterNot (`S.member` droplet) neighbours'
      neighbours''' = filterNot (`elem` nextToVisit) neighbours''
      nextToVisit' = nextToVisit ++ neighbours'''
  in getReachables' boundingArea droplet nextToVisit' visited'

getReachables :: BoundingArea -> Droplet -> Reachables
getReachables boundingArea droplet =
  let startPos = getStartPos boundingArea
      reachables = getReachables' boundingArea droplet [startPos] S.empty
  in reachables

countOuterFreeSides :: Droplet -> Int
countOuterFreeSides droplet =
  let boundingArea = getBoundingArea droplet
      outerPositions = getReachables boundingArea droplet
      numAdjacentsWithOuters = sum . map (countAdjacentPositions outerPositions) $ S.toList droplet
  in numAdjacentsWithOuters

day18 :: IO ()
day18 = do
  -- let input = testInput
  input <- readFile "input/Day18.txt"

  droplet <- case regularParse positionsParser input of
    Left msg -> fail $ show msg
    Right xs -> pure $ S.fromList xs

  -- part 1
  print $ countFreeSides droplet

  -- part 2
  print $ countOuterFreeSides droplet
