{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day24 where

import Control.Monad (when)
import Data.Heap (Heap)
import qualified Data.Heap as H
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Optics.Core (set)
import Optics.TH

import Util (filterNot)

testInput :: [String]
testInput = [
    "#.#####",
    "#.....#",
    "#>....#",
    "#.....#",
    "#...v.#",
    "#.....#",
    "#####.#"
  ]

testInput2 :: [String]
testInput2 = [
    "#.######",
    "#>>.<^<#",
    "#.<..<<#",
    "#>v.><>#",
    "#<^v^^>#",
    "######.#"
  ]

type X = Int
type Y = Int
type Pos = (X, Y)

data Direction = N | S | E | W
  deriving (Eq, Ord, Show)

getAdjacentPos :: Pos -> Direction -> Pos
getAdjacentPos (x, y) N = (x, y - 1)
getAdjacentPos (x, y) S = (x, y + 1)
getAdjacentPos (x, y) E = (x + 1, y)
getAdjacentPos (x, y) W = (x - 1, y)

data Move = Move Direction | Wait

possibleMoves :: [Move]
possibleMoves = [Move N, Move S, Move E, Move W, Wait]

applyMove :: Move -> Pos -> Pos
applyMove (Move d) pos = getAdjacentPos pos d
applyMove Wait pos = pos

data Field = Field {
  startPos :: Pos,
  endPos :: Pos,
  width :: Int,
  height :: Int,
  blizzards :: Map Pos [Direction]
} deriving (Eq, Show)

makeFieldLabelsNoPrefix ''Field

readField :: [String] -> Field
readField fieldLines = field
  where
    field = Field {
      startPos = startPos,
      endPos = endPos,
      width = maxX,
      height = maxY,
      blizzards = blizzards
    }
    blizzards = M.fromList $ catMaybes [
        fmap (\d -> ((x, y), [d])) (getDirection c)
        | (line, y) <- fieldLines `zip` [1..maxY],
          (c, x) <- line `zip` [1..maxX]
      ]
    startPos = head . catMaybes $ [if (fieldLines !! 0) !! (x - 1) == '.' then Just (x, 1) else Nothing | x <- [1..maxX]]
    endPos = head . catMaybes $ [if (fieldLines !! (maxY - 1)) !! (x - 1) == '.' then Just (x, maxY) else Nothing | x <- [1..maxX]]
    getDirection '^' = Just N
    getDirection 'v' = Just S
    getDirection '>' = Just E
    getDirection '<' = Just W
    getDirection _ = Nothing
    maxY = length fieldLines
    maxX = length (head fieldLines)

isBlizzardAt :: Field -> Pos -> Bool
isBlizzardAt field pos = M.member pos field.blizzards

isWallAt :: Field -> Pos -> Bool
isWallAt field pos@(x, y) =    x == 1
                            || x == field.width
                            || (y == 1 && pos /= field.startPos)
                            || (y == field.height && pos /= field.endPos)

getBlizzardPositions :: Field -> [Pos]
getBlizzardPositions field = map fst . M.toList $ field.blizzards

wrapPos :: Field -> Pos -> Direction -> Pos
wrapPos field (x, _) N = (x, field.height - 1)
wrapPos _ (x, _) S = (x, 2)
wrapPos _ (_, y) E = (2, y)
wrapPos field (_, y) W = (field.width - 1, y)

moveBlizzard :: Field -> Pos -> Direction -> Pos
moveBlizzard field pos d = let
    pos' = getAdjacentPos pos d
    pos'' = if isWallAt field pos' then wrapPos field pos d
            else pos'
  in pos''

insertBlizzard :: Map Pos [Direction] -> Pos -> Direction -> Map Pos [Direction]
insertBlizzard blizzards pos d =
  case M.lookup pos blizzards of
    Nothing -> M.insert pos [d] blizzards
    Just ds -> M.insert pos (d:ds) blizzards

moveBlizzardsAtPos :: Field -> Pos -> Field
moveBlizzardsAtPos field pos =
  case M.lookup pos field.blizzards of
    Nothing -> field
    Just blizzardDirections ->
      let
          blizzardPositions = map (\d -> moveBlizzard field pos d) blizzardDirections
          blizzards' = M.delete pos field.blizzards
          blizzards'' = foldl (\acc (p, d) -> insertBlizzard acc p d) blizzards' (blizzardPositions `zip` blizzardDirections)
          field' = set #blizzards blizzards'' field
      in field'

moveBlizzards :: Field -> Field
moveBlizzards field =
  let blizzards = getBlizzardPositions field
      field' = foldl (\acc p -> moveBlizzardsAtPos acc p) field blizzards
  in field'

type Minute = Int
type PathLength = Int

isOutside :: Pos -> Bool
isOutside (x, y) = y < 1

data SearchNode = SearchNode Pos PathLength Minute deriving Eq

getPathLength :: SearchNode -> Int
getPathLength (SearchNode _ pathLength _) = pathLength

-- Takes 1.5 seconds for the test input
instance Ord SearchNode where
 a <= b = getPathLength a <= getPathLength b

-- Does not finish in acceptable time
-- instance Ord SearchNode where
--   a <= b = getPathLength a > getPathLength b

getValidNextPositions :: Field -> Pos -> [Pos]
getValidNextPositions field pos = let
    moves = possibleMoves
    nextPositions1 = map (flip applyMove pos) moves
    nextPositions2 = filterNot (\p -> isOutside p) nextPositions1
    nextPositions3 = filterNot (\p -> isWallAt field p) nextPositions2
    nextPositions4 = filterNot (\p -> isBlizzardAt field p) nextPositions3
  in nextPositions4

go :: Heap SearchNode -> IORef Minute -> IORef PathLength -> Map Minute Field -> IO ()
go searchNodes minMinuteRef minPathLengthRef fields =
  case H.viewMin searchNodes of
    Nothing -> putStrLn "empty"
    Just ((SearchNode pos pathLength minute), searchNodes') -> do
      let field = fields M.! (minute+1)

      searchNodes'' <-
        if field.endPos == pos then do
          minPathLength <- readIORef minPathLengthRef
          when (pathLength < minPathLength) $ do
            writeIORef minPathLengthRef pathLength
            putStrLn $ "pathLength: " <> show pathLength

          minMinute <- readIORef minMinuteRef
          when (minute < minMinute) $ do
            writeIORef minMinuteRef minute
            putStrLn $ "minMinute: " <> show minute

          -- putStrLn $ "end: " <> show minute
          -- print $ map getPathLength (H.toUnsortedList searchNodes)
          -- return $ H.filter (\s -> getPathLength s <= pathLength) searchNodes'
          return searchNodes'
        else
          return searchNodes'

      -- putStrLn $ show pos <> " - " <> show pathLength <> " - " <> show path
      -- putStrLn $ show pos <> " - " <> show pathLength
      minPathLength <- readIORef minPathLengthRef
      let
        searchNodes''' =
          if pathLength + 1 <= minPathLength then
            let nextPositions = getValidNextPositions field pos
            in foldl (\acc p -> H.insert (SearchNode p (pathLength + 1) (minute + 1)) acc) searchNodes'' nextPositions
          else
            searchNodes''
      go searchNodes''' minMinuteRef minPathLengthRef fields

day24 :: IO ()
day24 = do
  -- let input = testInput
  let input = testInput2
  -- input <- lines <$> readFile "input/Day24.txt"

  putStrLn "day24"

  let field = readField input
  print field

  minMinuteRef <- newIORef 1000000
  minPathLengthRef <- newIORef 1000000

  let searchNodes = H.singleton (SearchNode field.startPos 0 1)
      fields = M.fromList $ scanl (\(_, acc) i -> (i, moveBlizzards acc)) (1, field) [2..100]

  go searchNodes minMinuteRef minPathLengthRef fields

  minPathLength <- readIORef minPathLengthRef
  minMinute <- readIORef minMinuteRef

  putStrLn $ "minPathLength: " <> show minPathLength
  putStrLn $ "minMinute: " <> show minMinute

  return ()
