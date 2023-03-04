module Day24 where

import Data.Map (Map)
import qualified Data.Map as M

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

type X = Int
type Y = Int
type Pos = (X, Y)

data Direction = N | S | E | W
  deriving (Eq, Show)

getAdjacentPos :: Pos -> Direction -> Pos
getAdjacentPos (x, y) N = (x, y - 1)
getAdjacentPos (x, y) S = (x, y + 1)
getAdjacentPos (x, y) E = (x + 1, y)
getAdjacentPos (x, y) W = (x - 1, y)

getOppositeDirection :: Direction -> Direction
getOppositeDirection N = S
getOppositeDirection S = N
getOppositeDirection E = W
getOppositeDirection W = E

data Tile
  = Wall
  | Floor
  | Blizzard Direction
  deriving (Eq, Show)

type Field = Map Pos Tile

readField :: [String] -> Field
readField fieldLines = M.fromList [
                         ((x, y), getTile c)
                         | (line, y) <- fieldLines `zip` [1..maxY],
                           (c, x) <- line `zip` [1..maxX]
                       ]
  where
    getTile '#' = Wall
    getTile '.' = Floor
    getTile '^' = Blizzard N
    getTile 'v' = Blizzard S
    getTile '>' = Blizzard E
    getTile '<' = Blizzard W
    getTile c = error $ "Invalid tile char: '" <> show c <> "'"
    maxY = length fieldLines
    maxX = length (head fieldLines)

isBlizzardAt :: Field -> Pos -> Bool
isBlizzardAt field pos =
  case M.lookup pos field of
    Just (Blizzard _) -> True
    _ -> False

isWallAt :: Field -> Pos -> Bool
isWallAt field pos =
  case M.lookup pos field of
    Just Wall -> True
    _ -> False

isFloorAt :: Field -> Pos -> Bool
isFloorAt field pos =
  case M.lookup pos field of
    Just Floor -> True
    _ -> False

wrapPos' :: Field -> Pos -> Direction -> Pos
wrapPos' field pos d =
  let pos' = getAdjacentPos pos d
  in if isWallAt field pos' then pos
     else wrapPos' field pos' d

wrapPos :: Field -> Pos -> Direction -> Pos
wrapPos field pos d = wrapPos' field pos (getOppositeDirection d)

moveBlizzard :: Field -> Pos -> Direction -> (Field, Pos)
moveBlizzard field pos d =
  let pos' = getAdjacentPos pos d
      pos'' = if isWallAt field pos then wrapPos field pos d
              else pos'
      field' = M.insert pos Floor field
      field'' = M.insert pos'' (Blizzard d) field'
  in (field'', pos'')

day24 :: IO ()
day24 = do
  let input = testInput

  putStrLn "day24"

  let field = readField input
  print field

  return ()
