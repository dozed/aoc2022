module Day17 (Jet(..), day17, jetsParser) where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String

testInput :: String
testInput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

data Jet = JetLeft | JetRight
           deriving (Eq, Show)

jetsParser :: Parser [Jet]
jetsParser = many1 $ (JetLeft <$ char '<') <|> (JetRight <$ char '>')

data Block = HLine
           | Plus
           | L
           | VLine
           | Square
           deriving (Eq, Show)

type X = Int
type Y = Int
type Pos = (X, Y)
type Field = Set Pos

getDownPos :: Pos -> Pos
getDownPos (x, y) = (x, y - 1)

materialize :: Block -> Pos -> Set Pos
materialize HLine (x, y) = S.fromList [(x, y), (x+1, y), (x+2, y), (x+3, y)]
materialize Plus (x, y) = S.fromList [(x+1, y), (x, y+1), (x+1, y+1), (x+2, y+1), (x+1, y+2)]
materialize L (x, y) = S.fromList [(x, y), (x+1, y), (x+2, y), (x+2, y+1), (x+2, y+2)]
materialize VLine (x, y) = S.fromList [(x, y), (x, y+1), (x, y+2), (x, y+3)]
materialize Square (x, y) = S.fromList [(x, y), (x+1, y), (x, y+1), (x+1, y+1)]

getMaxY :: Field -> Y
getMaxY = snd . maximumBy (compare `on` snd)

getStartPos :: Field -> Pos
getStartPos field = (2, getMaxY field + 4)

canMove :: (Pos -> Pos) -> Field -> (Block, Pos) -> Bool
canMove adjust field (block, pos) =
  let blockPos = materialize block pos
      blockPos' = S.map adjust blockPos
      isNotBlockedByFieldRock = S.disjoint field blockPos'
      isNotBlockedByLeftWall = all (\(x, _) -> x >= 1) blockPos'
      isNotBlockedByRightWall = all (\(x, _) -> x <= 7) blockPos'
      isNotBlockedByFloor = all (\(_, y) -> y >= 1) blockPos'
      isNotBlocked = isNotBlockedByFieldRock && isNotBlockedByLeftWall && isNotBlockedByRightWall && isNotBlockedByFloor
  in isNotBlocked

canMoveDown :: Field -> (Block, Pos) -> Bool
canMoveDown = canMove (\(x, y) -> (x, y - 1))

canMoveLeft :: Field -> (Block, Pos) -> Bool
canMoveLeft = canMove (\(x, y) -> (x - 1, y))

canMoveRight :: Field -> (Block, Pos) -> Bool
canMoveRight = canMove (\(x, y) -> (x + 1, y))

day17 :: IO ()
day17 = do
  putStrLn "day17"
