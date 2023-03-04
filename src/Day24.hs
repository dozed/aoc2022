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

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Optics.Core (set)
import Optics.TH

import Util (replaceAtIndex)

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
  deriving (Eq, Ord, Show)

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

data Field = Field {
  walls :: Set Pos,
  startPos :: Pos,
  endPos :: Pos,
  blizzards :: [(Pos, Direction)],
  blizzardsPos :: Set Pos
} deriving (Show)

makeFieldLabelsNoPrefix ''Field

instance Eq Field where
  a == b =
    a.walls == b.walls
    && a.startPos == b.startPos
    && a.endPos == b.endPos
    && sort a.blizzards == sort b.blizzards
    && a.blizzardsPos == b.blizzardsPos

readField :: [String] -> Field
readField fieldLines = field
  where
    field = Field {
      walls = walls,
      startPos = startPos,
      endPos = endPos,
      blizzards = blizzards,
      blizzardsPos = blizzardsPos
    }
    walls = S.fromList . catMaybes $ [
        if c == '#' then Just (x, y) else Nothing
        | (line, y) <- fieldLines `zip` [1..maxY],
          (c, x) <- line `zip` [1..maxX]
      ]
    blizzardsPos = S.fromList . map fst $ blizzards
    blizzards = catMaybes [
        fmap (\d -> ((x, y), d)) (getDirection c)
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
isBlizzardAt field pos = S.member pos field.blizzardsPos

isWallAt :: Field -> Pos -> Bool
isWallAt field pos = S.member pos field.walls

getBlizzards :: Field -> [(Pos, Direction)]
getBlizzards field = field.blizzards

wrapPos' :: Field -> Pos -> Direction -> Pos
wrapPos' field pos d =
  let pos' = getAdjacentPos pos d
  in if isWallAt field pos' then pos
     else wrapPos' field pos' d

wrapPos :: Field -> Pos -> Direction -> Pos
wrapPos field pos d = wrapPos' field pos (getOppositeDirection d)

mkBlizzardsPos :: [(Pos, Direction)] -> Set Pos
mkBlizzardsPos blizzards = S.fromList $ map fst blizzards

moveBlizzard :: Field -> Int -> (Field, Pos)
moveBlizzard field idx =
  let blizzards = field.blizzards
      (pos, d) = blizzards !! idx
      pos' = getAdjacentPos pos d
      pos'' = if isWallAt field pos' then wrapPos field pos d
              else pos'
      blizzards' = replaceAtIndex idx (pos'', d) blizzards
      blizzardsPos' = mkBlizzardsPos blizzards'
      field' = set #blizzards blizzards' field
      field'' = set #blizzardsPos blizzardsPos' field'
  in (field'', pos'')

moveBlizzard' :: Field -> Int -> Field
moveBlizzard' field idx = fst $ moveBlizzard field idx

moveBlizzards :: Field -> Field
moveBlizzards field =
  let numBlizzards = length . getBlizzards $ field
      field' = foldl (\acc i -> moveBlizzard' acc i) field [0..(numBlizzards-1)]
  in field'

day24 :: IO ()
day24 = do
  let input = testInput

  putStrLn "day24"

  let field = readField input
  print field

  return ()
