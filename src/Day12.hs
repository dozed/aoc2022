{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day12 where

import Control.Monad (mfilter)
import Data.Char (chr, ord)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, catMaybes)
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

getPathLength :: Path -> Int
getPathLength p = length p - 1

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

getPathTo' :: PredecessorMap -> Pos -> Path -> Path
getPathTo' predecessors currentPos pathSoFar = do
  case M.lookup currentPos predecessors of
    Just x -> getPathTo' predecessors x (currentPos:pathSoFar)
    Nothing -> currentPos:pathSoFar

getPathTo :: PredecessorMap -> Pos -> Pos -> Maybe Path
getPathTo predecessors startPos endPos =
  let path = getPathTo' predecessors endPos []
      pathFiltered = if head path == startPos then Just path else Nothing
  in pathFiltered

searchShortestPathsBfsFrom :: Field -> Pos -> Pos -> Maybe Path
searchShortestPathsBfsFrom field startPos endPos =
  let predecessors = searchShortestPathsBfsFrom' field startPos [] S.empty M.empty
      shortestPath = getPathTo predecessors startPos endPos
  in shortestPath

getPositionsOnHeight :: Field -> CellHeight -> [Pos]
getPositionsOnHeight field ch = map fst . filter (\(p, c) -> c == ch) . M.toList $ field

day12 :: IO ()
day12 = do
  let input = testInput1
  -- input <- strip <$> readFile "input/Day12.txt"

  let field = mkField input

  startPos <- case getStartPos field of
    Nothing -> fail "could not get start position"
    (Just pos) -> pure pos

  endPos <- case getEndPos field of
    Nothing -> fail "could not get end position"
    (Just pos) -> pure pos

  -- debug
  putStrLn "Field:"
  putStrLn input

  putStrLn $ "startPos: " <> show startPos
  putStrLn $ "endPos: " <> show endPos

  -- part 1
  shortestPath <- case searchShortestPathsBfsFrom field startPos endPos of
    Nothing -> fail "could not get shortest path"
    Just x -> pure x

  let shortestPathLength = getPathLength shortestPath

  putStrLn $ "Shortest path: " <> show shortestPath
  putStrLn $ "Shortest path length: " <> show shortestPathLength

  -- part 2
  let possibleStartPositions = getPositionsOnHeight field 'a'
      allPaths = catMaybes . filter isJust . map (\x -> searchShortestPathsBfsFrom field x endPos) $ possibleStartPositions
      shortestPathLength = minimum . map getPathLength $ allPaths

  putStrLn $ "Shortest path length: " <> show shortestPathLength
