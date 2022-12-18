{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day12 where

import Control.Monad (mfilter)
import Data.Char (chr, ord)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Text.RawString.QQ

import Util (strip)

testInput1 :: String
testInput1 = strip [r|
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
|]

type Cell = Char
type CellHeight = Char
type Y = Int
type X = Int
type Pos = (Y, X)
type Field = Map Pos Cell
type Path = [Pos]

isStart :: Cell -> Bool
isStart 'S' = True
isStart _ = False

isEnd :: Cell -> Bool
isEnd 'E' = True
isEnd _ = False

toHeight :: Cell -> CellHeight
toHeight c
  | isStart c = 'a'
  | isEnd c = 'z'
  | otherwise = c

incrCellHeight :: CellHeight -> CellHeight
incrCellHeight c = chr (ord c + 1)

mkField :: String -> Field
mkField txt =
  let xs = lines txt
      height = length xs
      width = length . head $ xs
      positions = [(y, x) | y <- [0..height-1], x <- [0..width-1]]
      values = [(pos, (xs !! y) !! x) | pos@(y, x) <- positions]
      map = M.fromList values
  in map

getPos :: (Cell -> Bool) -> Field -> Maybe Pos
getPos checkCell field = fst <$> find (\(_, cell) -> checkCell cell) (M.toList field)

getStartPos :: Field -> Maybe Pos
getStartPos = getPos isStart

getEndPos :: Field -> Maybe Pos
getEndPos = getPos isEnd

getCellValue :: Field -> Pos -> Maybe Cell
getCellValue fieldMap pos = M.lookup pos fieldMap

getCellHeight :: Field -> Pos -> Maybe CellHeight
getCellHeight field pos = fmap toHeight . getCellValue field $ pos

getAdjacentPositions :: Pos -> Set Pos
getAdjacentPositions (y, x) = S.fromList [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]

getReachablePositions :: Field -> Pos -> Set Pos
getReachablePositions field pos =
  case getCellHeight field pos of
    Nothing -> S.empty
    Just currentCellHeight ->
      let
        maxReachableCellHeight = incrCellHeight currentCellHeight
        adjacentPositions = getAdjacentPositions pos
        isReachable pos = isJust . mfilter (<= maxReachableCellHeight) . getCellHeight field $ pos
        reachablePositions = S.filter isReachable adjacentPositions
      in reachablePositions

type PredecessorMap = Map Pos Pos

searchShortestPathsBfsFrom' :: Field -> Pos -> [Pos] -> Set Pos -> PredecessorMap -> PredecessorMap
searchShortestPathsBfsFrom' field pos toVisit visited predecessors =
  let visited' = S.insert pos visited
      reachablePositions = getReachablePositions field pos
      reachablePositionsNotVisited = S.difference reachablePositions visited'
      reachablePositionsNotVisitedAndNotToVisit = S.difference reachablePositionsNotVisited (S.fromList toVisit)
      toVisit' = toVisit ++ S.toList reachablePositionsNotVisitedAndNotToVisit
      predecessors' = foldl (\acc x -> M.insert x pos acc) predecessors reachablePositionsNotVisitedAndNotToVisit
      -- toVisit' = toVisit ++ S.toList reachablePositionsNotVisited
      -- predecessors' = foldl (\acc x -> M.insert x pos acc) predecessors reachablePositionsNotVisited

  in case toVisit' of
    [] -> predecessors'
    x:xs -> searchShortestPathsBfsFrom' field x xs visited' predecessors'

searchShortestPathsBfsFrom :: Field -> Pos -> PredecessorMap
searchShortestPathsBfsFrom field startPos = searchShortestPathsBfsFrom' field startPos [] S.empty M.empty

getPathTo' :: Map Pos Pos -> Pos -> Path -> Path
getPathTo' predecessors pos pathSoFar = do
  case M.lookup pos predecessors of
    Just x -> getPathTo' predecessors x (pos:pathSoFar)
    Nothing -> pos:pathSoFar

getPathTo :: Map Pos Pos -> Pos -> Path
getPathTo predecessors pos = getPathTo' predecessors pos []

day12 :: IO ()
day12 = do
  -- let input = testInput1
  input <- strip <$> readFile "input/Day12.txt"

  let field = mkField input

  startPos <- case getStartPos field of
    (Just pos) -> pure pos
    Nothing -> fail "could not get start position"

  endPos <- case getEndPos field of
    (Just pos) -> pure pos
    Nothing -> fail "could not get end position"

  -- debug
  putStrLn "Field:"
  putStrLn input

  putStrLn $ "startPos: " <> show startPos
  putStrLn $ "endPos: " <> show endPos

  -- part 1
  let predecessors = searchShortestPathsBfsFrom field startPos
      shortestPath = getPathTo predecessors endPos
      shortestPathLength = length shortestPath - 1

  putStrLn $ "Shortest path: " <> show shortestPath
  putStrLn $ "Shortest path length: " <> show shortestPathLength
