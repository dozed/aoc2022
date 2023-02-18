{-# LANGUAGE QuasiQuotes #-}

module Day22 where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, minimumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Text.RawString.QQ

testInput :: String
testInput = tail [r|
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
|]

type X = Int
type Y = Int
type Pos = (X, Y)

data Tile = Floor | Wall | Empty
            deriving (Eq, Show)

type Field = Map Pos Tile

data Move = TurnLeft | TurnRight | MoveForward Int
            deriving (Eq, Show)

readField :: [[Char]] -> Field
readField = readRows
  where
    readRows rows = foldl (\acc (row, y) -> readRow acc y row) M.empty (rows `zip` [1..])
    readRow field y row = foldl (\acc (c, x) -> insertTile acc (x, y) (getTile c)) field (row `zip` [1..])
    insertTile acc (x, y) (Just tile) = M.insert (x, y) tile acc
    insertTile acc (_, _) Nothing = acc
    getTile '.' = Just Floor
    getTile '#' = Just Wall
    getTile _ = Nothing

getStartPos :: Field -> Pos
getStartPos field =
  let ps = M.keys field
      ps' = filter (\(_, y) -> y == 1) ps
      sp = minimumBy (compare `on` fst) ps'
  in sp

parseMove :: String -> Move
parseMove "L" = TurnLeft
parseMove "R" = TurnRight
parseMove str = MoveForward $ read str

parseMoves :: String -> [Move]
parseMoves str =
  let groups = groupBy (\a b -> isDigit a && isDigit b) str
      moves = map parseMove groups
  in moves

data Orientation = U | D | L | R
                   deriving (Eq, Show)

reorient :: Orientation -> Move -> Orientation
reorient U TurnLeft = L
reorient U TurnRight = R
reorient D TurnLeft = R
reorient D TurnRight = L
reorient L TurnLeft = D
reorient L TurnRight = U
reorient R TurnLeft = U
reorient R TurnRight = D
reorient o _ = o

getOppositeOrient :: Orientation -> Orientation
getOppositeOrient U = D
getOppositeOrient D = U
getOppositeOrient L = R
getOppositeOrient R = L

getNextPos :: Pos -> Orientation -> Pos
getNextPos (x, y) U = (x, y - 1)
getNextPos (x, y) D = (x, y + 1)
getNextPos (x, y) L = (x - 1, y)
getNextPos (x, y) R = (x + 1, y)

getTile :: Field -> Pos -> Tile
getTile field pos = case M.lookup pos field of
  Just Wall -> Wall
  Just Floor -> Floor
  Nothing -> Empty
  Just Empty -> Empty

isFloor :: Field -> Pos -> Bool
isFloor field pos = case M.lookup pos field of
  Nothing -> False
  Just Wall -> False
  Just Empty -> False
  Just Floor -> True

isWall :: Field -> Pos -> Bool
isWall field pos = case M.lookup pos field of
  Nothing -> False
  Just Floor -> False
  Just Empty -> False
  Just Wall -> True

isEmpty :: Field -> Pos -> Bool
isEmpty field pos = case M.lookup pos field of
  Just Floor -> False
  Just Wall -> False
  Just Empty -> True
  Nothing -> True

getOppositePos :: Field -> Pos -> Orientation -> Pos
getOppositePos field pos orient =
  let pos' = getNextPos pos orient
      pos'' = case getTile field pos' of
        Empty -> pos
        Wall -> getOppositePos field pos' orient
        Floor -> getOppositePos field pos' orient
  in pos''

go :: Field -> Pos -> Orientation -> Move -> (Pos, Orientation)
go _ pos orient TurnLeft = (pos, reorient orient TurnLeft)
go _ pos orient TurnRight = (pos, reorient orient TurnRight)
go _ pos orient (MoveForward 0) = (pos, orient)
go field pos orient (MoveForward n) =
  let pos' = getNextPos pos orient
      pos'' = case getTile field pos of
        Floor -> pos'
        Wall -> pos
        Empty ->
          let opOrient = getOppositeOrient orient
              opPos = getOppositePos field pos opOrient
          in if isWall field opPos then pos
             else opPos
  in go field pos'' orient (MoveForward (n - 1))

day22 :: IO ()
day22 = do
  let input = testInput

  putStrLn "day22"

  let xs = lines input

  let moves = last xs
      field = init . init $ xs

  putStrLn $ "field:" <> show field
  putStrLn $ "moves: " <> moves

  let field' = readField field
  print field'

  let startPos = getStartPos field'
  putStrLn $ "startPos: " <> show startPos

  return ()
