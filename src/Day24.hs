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

import Control.Concurrent (threadDelay)
import Data.Char (chr, ord)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Optics.Core (set)
import Optics.TH
import System.Posix.Internals (puts)

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

directionFromChar :: Char -> Maybe Direction
directionFromChar '^' = Just N
directionFromChar 'v' = Just S
directionFromChar '>' = Just E
directionFromChar '<' = Just W
directionFromChar _ = Nothing

directionToChar :: Direction -> Char
directionToChar N = '^'
directionToChar S = 'v'
directionToChar E = '>'
directionToChar W = '<'

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

insertOne :: Ord k => Map k [v] -> k -> v -> Map k [v]
insertOne values k v =
  case M.lookup k values of
    Nothing -> M.insert k [v] values
    Just vs -> M.insert k (v:vs) values

insertAll :: Ord k => Map k [v] -> k -> [v] -> Map k [v]
insertAll values k vs = foldl (\acc v -> insertOne acc k v) values vs

glueTwo :: Ord k => Map k [v] -> Map k [v] -> Map k [v]
glueTwo a b = foldl (\acc (k, vs) -> insertAll acc k vs) a (M.toList b)

glueAll :: Ord k => [Map k [v]] -> Map k [v]
glueAll [] = M.empty
glueAll [x] = x
glueAll (x:y:zs) = glueAll ((glueTwo x y):zs)

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
        fmap (\d -> ((x, y), [d])) (directionFromChar c)
        | (line, y) <- fieldLines `zip` [1..maxY],
          (c, x) <- line `zip` [1..maxX]
      ]
    startPos = head . catMaybes $ [if (fieldLines !! 0) !! (x - 1) == '.' then Just (x, 1) else Nothing | x <- [1..maxX]]
    endPos = head . catMaybes $ [if (fieldLines !! (maxY - 1)) !! (x - 1) == '.' then Just (x, maxY) else Nothing | x <- [1..maxX]]
    maxY = length fieldLines
    maxX = length (head fieldLines)

showField :: Field -> Set Pos -> String
showField field waveFront = unlines xs
  where
    xs = [[getCh x y | x <- [1..field.width]] | y <- [1..field.height]]
    getCh x y = if isWallAt field (x, y) then '#'
                else if S.member (x, y) waveFront then 'o'
                else
                  case M.lookup (x, y) field.blizzards of
                    Nothing -> '.'
                    Just [d] -> directionToChar d
                    Just ds -> intToChar $ length ds
    intToChar x = if 0 < x && x < 10 then chr (ord '0' + x)
                  else '@'

printField :: Field -> Set Pos -> IO ()
printField field waveFront = puts $ "\^[c\n" <> showField field waveFront

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

moveBlizzardsAtPos :: Field -> Pos -> [Direction] -> Map Pos [Direction]
moveBlizzardsAtPos field blizzardPosition blizzardDirections =
  let blizzardPositions' = map (\d -> moveBlizzard field blizzardPosition d) blizzardDirections
      blizzards = foldl (\acc (p, d) -> insertOne acc p d) M.empty (blizzardPositions' `zip` blizzardDirections)
  in blizzards

moveBlizzards :: Field -> Field
moveBlizzards field =
  let blizzardMaps = map (\(p, ds) -> moveBlizzardsAtPos field p ds) $ M.toList field.blizzards
      blizzards = glueAll blizzardMaps
      field' = set #blizzards blizzards field
  in field'

type Minute = Int
type PathLength = Int

isOutside :: Pos -> Bool
isOutside (x, y) = y < 1

getValidNextPositions :: Field -> Pos -> [Pos]
getValidNextPositions field pos = let
    moves = possibleMoves
    nextPositions1 = map (flip applyMove pos) moves
    nextPositions2 = filterNot (\p -> isOutside p) nextPositions1
    nextPositions3 = filterNot (\p -> isWallAt field p) nextPositions2
    nextPositions4 = filterNot (\p -> isBlizzardAt field p) nextPositions3
  in nextPositions4

go :: Set Pos -> Minute -> Field -> Pos -> IO (Field, Minute)
go waveFront minute field endPos | S.member endPos waveFront = return (field, minute)
go waveFront minute field endPos = do
  let field' = moveBlizzards field
  let waveFront' = S.unions . S.map (\pos -> S.fromList $ getValidNextPositions field' pos) $ waveFront
  printField field' waveFront'
  threadDelay 30000
  go waveFront' (minute+1) field' endPos

day24 :: IO ()
day24 = do
  -- let input = testInput
  -- let input = testInput2
  input <- lines <$> readFile "input/Day24.txt"

  putStrLn "day24"

  let field = readField input
  print field

  -- part 1
  (field', minute) <- go (S.singleton field.startPos) 0 field field.endPos
  putStrLn $ "part 1: " <> show minute
  
  -- part 2
  (field'', minute') <- go (S.singleton field.endPos) minute field' field.startPos
  (_, minute'') <- go (S.singleton field.startPos) minute' field'' field.endPos
  putStrLn $ "part 2: " <> show minute''

  return ()
