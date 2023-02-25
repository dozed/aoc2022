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

type FieldIndex = Int
type FieldMap = Map FieldIndex Field

data Tile = Floor | Wall | Empty
            deriving (Eq, Show)

type Field = Map Pos Tile

data Move = TurnLeft | TurnRight | MoveForward Int
            deriving (Eq, Show)

data Orientation = U | D | L | R
                   deriving (Eq, Ord, Show)

readField :: [[Char]] -> Field
readField = readRows
  where
    readRows rows = foldl (\acc (row, y) -> readRow acc y row) M.empty (rows `zip` [1..])
    readRow field y row = foldl (\acc (c, x) -> insertTile acc (x, y) (readTile c)) field (row `zip` [1..])
    insertTile acc (x, y) (Just tile) = M.insert (x, y) tile acc
    insertTile acc (_, _) Nothing = acc
    readTile '.' = Just Floor
    readTile '#' = Just Wall
    readTile _ = Nothing

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

reorient :: Move -> Orientation -> Orientation
reorient TurnLeft  U = L
reorient TurnRight U = R
reorient TurnLeft  D = R
reorient TurnRight D = L
reorient TurnLeft  L = D
reorient TurnRight L = U
reorient TurnLeft  R = U
reorient TurnRight R = D
reorient _ o = o

getOppositeOrient :: Orientation -> Orientation
getOppositeOrient = reorient TurnLeft . reorient TurnLeft

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
isFloor field pos = getTile field pos == Floor

isWall :: Field -> Pos -> Bool
isWall field pos = getTile field pos == Wall

isEmpty :: Field -> Pos -> Bool
isEmpty field pos = getTile field pos == Empty

getOppositePos :: Field -> Pos -> Orientation -> Pos
getOppositePos field pos orient =
  let pos' = getNextPos pos orient
      pos'' = case getTile field pos' of
        Empty -> pos
        Wall -> getOppositePos field pos' orient
        Floor -> getOppositePos field pos' orient
  in pos''

go :: Field -> Pos -> Orientation -> Move -> (Pos, Orientation)
go _ pos orient TurnLeft = (pos, reorient TurnLeft orient)
go _ pos orient TurnRight = (pos, reorient TurnRight orient)
go _ pos orient (MoveForward 0) = (pos, orient)
go field pos orient (MoveForward n) =
  let pos' = getNextPos pos orient
      pos'' = case getTile field pos' of
        Floor -> pos'
        Wall -> pos
        Empty ->
          let opOrient = getOppositeOrient orient
              opPos = getOppositePos field pos opOrient
          in if isWall field opPos then pos
             else opPos
  in go field pos'' orient (MoveForward (n - 1))

getFacing :: Orientation -> Int
getFacing U = 3
getFacing D = 1
getFacing L = 2
getFacing R = 0

getPassword :: Pos -> Orientation -> Int
getPassword (x, y) orient = 1000 * y + 4 * x + getFacing orient

day22 :: IO ()
day22 = do
  -- let input = testInput
  input <- readFile "input/Day22.txt"

  putStrLn "day22"

  let xs = lines input

  let moves = last xs
      field = init . init $ xs

  putStrLn $ "field:" <> show field
  putStrLn $ "moves: " <> moves

  let field' = readField field
  print field'

  let moves' = parseMoves moves

  let startPos = getStartPos field'
      startOrient = R

  putStrLn $ "startPos: " <> show startPos
  putStrLn $ "startOrient: " <> show startOrient

  let (finalPos, finalOrient) = foldl (\(pos, orient) move -> go field' pos orient move) (startPos, startOrient) moves'

  putStrLn $ "finalPos: " <> show finalPos
  putStrLn $ "finalOrient: " <> show finalOrient
  putStrLn $ "password: " <> show (getPassword finalPos finalOrient)

  return ()
