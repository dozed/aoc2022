{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day12 where

import Control.Applicative ((<|>))
import Control.Monad (forM_, mfilter)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (findIndex, minimumBy)
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe, isJust)
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
type Field = [[Cell]]
type Y = Int
type X = Int
type Pos = (Y, X)
type Path = [Pos]

isStart :: Cell -> Bool
isStart 'S' = True
isStart _ = False

isEnd :: Cell -> Bool
isEnd 'E' = True
isEnd _ = False

getHeight :: Cell -> Cell
getHeight c
  | isStart c = 'a'
  | isEnd c = 'z'
  | otherwise = c

incrCell :: Cell -> Cell
incrCell c = chr (ord c + 1)

mkField :: String -> Field
mkField = lines

getPos :: (Cell -> Bool) -> Field -> Maybe Pos
getPos checkCell field = getPos' 0 field
  where
    getPos' :: Y -> Field -> Maybe Pos
    getPos' rowIdx (row:others) =
      let idx = (\colIdx -> (rowIdx, colIdx)) <$> findIndex checkCell row
      in idx <|> getPos' (rowIdx+1) others
    getPos' _ [] = Nothing

getStartPos :: Field -> Maybe Pos
getStartPos = getPos isStart

getEndPos :: Field -> Maybe Pos
getEndPos = getPos isEnd

getCell :: Field -> Pos -> Maybe Cell
getCell field (y, x) = do
  row <- field !? y
  cell <- row !? x
  return cell

getAdjacentPositions :: Pos -> Set Pos
getAdjacentPositions (y, x) = S.fromList [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]

getReachablePositions :: Field -> Pos -> Set Pos
getReachablePositions field pos =
  case getCell field pos of
    Nothing -> S.empty
    Just currentCellValue ->
      let
        testCellValue = incrCell $ getHeight currentCellValue
        adjacentPositions = getAdjacentPositions pos
        isReachable pos = isJust . mfilter (<= testCellValue) . fmap getHeight . getCell field $ pos
        reachablePositions = S.filter isReachable adjacentPositions
      in reachablePositions

searchPaths' :: Field -> Pos -> [Pos] -> Set Pos -> [Path]
searchPaths' field toVisit currentPath visited =
  let cellValue = fromMaybe undefined $ getCell field toVisit
      currentPath' = toVisit : currentPath
      reachablePositions = getReachablePositions field toVisit
      isNotVisited pos = not . S.member pos $ visited
      positionsToVisit = S.filter isNotVisited reachablePositions
      visited' = foldl (flip S.insert) visited positionsToVisit
      otherPaths :: [Path]
        | isEnd cellValue = [reverse currentPath']
        | null positionsToVisit = []
        | otherwise = concatMap (\x -> searchPaths' field x currentPath' visited') positionsToVisit
  in otherPaths

searchPaths :: Field -> Pos -> [Path]
searchPaths field startPos = searchPaths' field startPos [] (S.singleton startPos)

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
  forM_ field $ \l -> do
    putStrLn l

  putStrLn $ "startPos: " <> show startPos
  putStrLn $ "endPos: " <> show endPos

  -- part 1
  let paths = searchPaths field startPos
      shortestPath = minimumBy (compare `on` length) paths
      shortestPathLength = length shortestPath
      shortestPathSteps = shortestPathLength - 1

  putStrLn $ "Shortest path: " <> show shortestPath
  putStrLn $ "Shortest path length: " <> show shortestPathLength
  putStrLn $ "Num steps: " <> show shortestPathSteps
