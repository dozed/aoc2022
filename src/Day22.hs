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

data Tile = Floor | Wall | Empty
            deriving (Eq, Show)

type Field = Map Pos Tile

type Side = Int
type SideSize = Int

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

getNew :: GetNew -> Pos -> SideSize -> Int
getNew TakeOne _ _ = 1
getNew TakeSize _ size = size
getNew TakeRow (_, y) _ = y
getNew TakeColumn (x, _) _ = x
getNew FlipRow (_, y) size = size - y + 1
getNew FlipColumn (x, _) size = size - x + 1

type Connections = Map (Side, Orientation) (Side, [Move], GetNewColumn, GetNewRow)

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

testExternalFieldPos :: Map Side Pos
testExternalFieldPos = M.fromList [
    (1, (9, 1)),
    (2, (1, 5)),
    (3, (5, 5)),
    (4, (9, 5)),
    (5, (9, 9)),
    (6, (13, 9))
  ]

getSidePos :: Map Side Pos -> Side -> Pos -> Pos
getSidePos fieldPos side (x, y) =
  let (fx, fy) = fromJust . M.lookup side $ fieldPos
      x' = x - fx + 1
      y' = y - fy + 1
  in (x', y')

getFieldPos :: Map Side Pos -> Side -> Pos -> Pos
getFieldPos fieldPos side (x, y) =
  let (fx, fy) = fromJust . M.lookup side $ fieldPos
      x' = x + fx - 1
      y' = y + fy - 1
  in (x', y')

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

isOutsideSide :: SideSize -> Pos -> Bool
isOutsideSide sideSize (x, y) =
  if x < 1 || x > sideSize then True
  else if y < 1 || y > sideSize then True
  else False

go2 :: Field -> SideSize -> Connections -> Map Side Pos -> Side -> Pos -> Orientation -> Move -> (Pos, Orientation)
go2 _ _ _ _ _ pos orient TurnLeft = (pos, reorient TurnLeft orient)
go2 _ _ _ _ _ pos orient TurnRight = (pos, reorient TurnRight orient)
go2 _ _ _ _ _ pos orient (MoveForward 0) = (pos, orient)
go2 field sideSize connections fieldPos side pos orient (MoveForward n) =
  let pos' = getNextPos pos orient
      lpos = getSidePos fieldPos side pos'
  in if isOutsideSide sideSize lpos then
       let (side', modOrient, getNewColumn, getNewRow) = fromJust . M.lookup (side, orient) $ connections
           lpos' = getSidePos fieldPos side pos
           x' = getNew getNewColumn lpos' sideSize
           y' = getNew getNewRow lpos' sideSize
           gpos = getFieldPos fieldPos side' (x', y')
           orient' = foldl (flip reorient) orient modOrient
           (pos'', orient'', side'') =
             if isWall field gpos then (pos, orient, side)
             else (gpos, orient', side')
       in go2 field sideSize connections fieldPos side'' pos'' orient'' (MoveForward (n - 1))
     else
       let
         pos'' = case getTile field pos' of
           Floor -> pos'
           Wall -> pos
           Empty -> error "Should not reach empty field pos"
       in go2 field sideSize connections fieldPos side pos'' orient (MoveForward (n - 1))

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
  let sideSize = 4
      startSide = 1
      connections = testConnections
      fieldPos = testExternalFieldPos
  let (finalPos', finalOrient') =
        foldl (\(pos, orient) move -> go2 field sideSize connections fieldPos startSide pos orient move)
              (startPos, startOrient) moves

  putStrLn "--- part 2 ---"
  putStrLn $ "finalPos: " <> show finalPos'
  putStrLn $ "finalOrient: " <> show finalOrient'
  putStrLn $ "password: " <> show (getPassword finalPos' finalOrient')

  return ()
