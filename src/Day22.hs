{-# LANGUAGE QuasiQuotes #-}

module Day22 where

import Data.Function (on)
import Data.List (minimumBy)
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

data Tile = Floor | Wall
            deriving (Eq, Show)

type Field = Map Pos Tile

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

day22 :: IO ()
day22 = do
  let input = testInput

  putStrLn "day22"

  let xs = lines input

  let cmds = last xs
      field = init . init $ xs

  putStrLn $ "field:" <> show field
  putStrLn $ "cmds: " <> cmds

  let field' = readField field
  print field'

  let startPos = getStartPos field'
  putStrLn $ "startPos: " <> show startPos

  return ()
