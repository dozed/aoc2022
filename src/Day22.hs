{-# LANGUAGE QuasiQuotes #-}

module Day22 where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, minimumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
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
type SubFieldSize = Int

data Tile = Floor | Wall | Empty
            deriving (Eq, Show)

type Field = Map Pos Tile

data Move = TurnLeft | TurnRight | MoveForward Int
            deriving (Eq, Show)

data Orientation = U | D | L | R
                   deriving (Eq, Ord, Show)

data GetNew = TakeOne
            | TakeSize
            | TakeRow
            | TakeColumn
            | FlipRow
            | FlipColumn

type GetNewColumn = GetNew
type GetNewRow = GetNew

getNew :: GetNew -> Pos -> SubFieldSize -> Int
getNew TakeOne _ _ = 1
getNew TakeSize _ size = size
getNew TakeRow (_, y) _ = y
getNew TakeColumn (x, _) _ = x
getNew FlipRow (_, y) size = size - y + 1
getNew FlipColumn (x, _) size = size - x + 1

type Connections = Map (FieldIndex, Orientation) (FieldIndex, [Move], GetNewColumn, GetNewRow)

testConnections :: Connections
testConnections = M.fromList [
    ((1, R), (6, [TurnLeft, TurnLeft], TakeSize, FlipRow)),
    ((1, D), (4, [], TakeColumn, TakeOne)),
    ((1, L), (3, [TurnLeft], TakeRow, TakeOne)),
    ((1, U), (2, [TurnLeft, TurnLeft], FlipColumn, TakeOne)),
    ((2, R), (3, [], TakeOne, TakeRow)),
    ((2, D), (5, [TurnLeft, TurnLeft], FlipRow, TakeSize)),
    ((2, L), (6, [TurnLeft], FlipRow, TakeSize)),
    ((2, U), (1, [TurnLeft, TurnLeft], FlipColumn, TakeOne)),
    ((3, L), (2, [], TakeSize, TakeRow)),
    ((3, R), (4, [], TakeOne, TakeRow)),
    ((3, D), (5, [TurnRight], TakeOne, TakeColumn)),
    ((3, U), (1, [TurnRight], TakeOne, TakeColumn)),
    ((4, L), (3, [], TakeSize, TakeRow)),
    ((4, U), (1, [], TakeColumn, TakeSize)),
    ((4, D), (5, [], TakeColumn, TakeOne)),
    ((4, R), (6, [TurnRight], FlipRow, TakeOne)),
    ((5, R), (6, [], TakeOne, TakeRow)),
    ((5, U), (4, [], TakeColumn, TakeSize)),
    ((5, L), (3, [TurnLeft], FlipRow, TakeSize)),
    ((5, D), (2, [TurnLeft, TurnLeft], FlipColumn, TakeSize)),
    ((6, L), (5, [], TakeSize, TakeColumn)),
    ((6, U), (4, [TurnLeft], TakeSize, FlipColumn)),
    ((6, R), (1, [TurnLeft, TurnLeft], TakeSize, FlipRow)),
    ((6, D), (2, [TurnLeft], TakeOne, FlipColumn))
  ]

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

go2 :: Field -> SubFieldSize -> Connections -> FieldIndex -> Pos -> Orientation -> Move -> (Pos, Orientation)
go2 _ _ _ _ pos orient TurnLeft = (pos, reorient TurnLeft orient)
go2 _ _ _ _ pos orient TurnRight = (pos, reorient TurnRight orient)
go2 _ _ _ _ pos orient (MoveForward 0) = (pos, orient)
go2 field subFieldSize connections fieldIndex pos orient (MoveForward n) =
  let pos' = getNextPos pos orient
      (pos'', orient'', fieldIndex'') = case getTile field pos' of
        Floor -> (pos', orient, fieldIndex)
        Wall -> (pos, orient, fieldIndex)
        Empty ->
          let (fieldIndex', modOrient, getNewColumn, getNewRow) = fromJust . M.lookup (fieldIndex, orient) $ connections
              x' = getNew getNewColumn pos subFieldSize
              y' = getNew getNewRow pos subFieldSize
              orient' = foldl (flip reorient) orient modOrient
          in if isWall field (x', y') then (pos, orient, fieldIndex)
             else ((x', y'), orient', fieldIndex')
  in go2 field subFieldSize connections fieldIndex'' pos'' orient'' (MoveForward (n - 1))

getFacing :: Orientation -> Int
getFacing U = 3
getFacing D = 1
getFacing L = 2
getFacing R = 0

getPassword :: Pos -> Orientation -> Int
getPassword (x, y) orient = 1000 * y + 4 * x + getFacing orient

day22 :: IO ()
day22 = do
  let input = testInput
  -- input <- readFile "input/Day22.txt"

  putStrLn "day22"

  let xs = lines input

  let movesLine = last xs
      fieldLines = init . init $ xs

  putStrLn $ "field:" <> show fieldLines
  putStrLn $ "moves: " <> movesLine

  let field = readField fieldLines
  print field

  let moves = parseMoves movesLine

  let startPos = getStartPos field
      startOrient = R

  putStrLn $ "startPos: " <> show startPos
  putStrLn $ "startOrient: " <> show startOrient

  -- part 1
  let (finalPos, finalOrient) = foldl (\(pos, orient) move -> go field pos orient move) (startPos, startOrient) moves

  putStrLn "--- part 1 ---"
  putStrLn $ "finalPos: " <> show finalPos
  putStrLn $ "finalOrient: " <> show finalOrient
  putStrLn $ "password: " <> show (getPassword finalPos finalOrient)

  -- part 2
  let subFieldSize = 4
      startFieldIndex = 1
      connections = testConnections
  let (finalPos', finalOrient') =
        foldl (\(pos, orient) move -> go2 field subFieldSize connections startFieldIndex pos orient move) (startPos, startOrient) moves

  putStrLn "--- part 2 ---"
  putStrLn $ "finalPos: " <> show finalPos'
  putStrLn $ "finalOrient: " <> show finalOrient'
  putStrLn $ "password: " <> show (getPassword finalPos' finalOrient')

  return ()
