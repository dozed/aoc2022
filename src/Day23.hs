{-# LANGUAGE LambdaCase #-}

module Day23 where

import Data.Function (on)
import Data.List (find, minimumBy, maximumBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isNothing)
import Data.Set (Set)
import qualified Data.Set as S

import Util (histogram)

testInput :: [String]
testInput = [
    "....#..",
    "..###.#",
    "#...#.#",
    ".#...##",
    "#.###..",
    "##.#.##",
    ".#..#.."
  ]

data Direction = N | S | E | W | NE | NW | SE | SW
                 deriving (Eq, Show)

allDirections :: [Direction]
allDirections = [N, S, E, W, NE, NW, SE, SW]

type X = Int
type Y = Int
type Pos = (X, Y)

type ElvesSet = Set Pos

readElves :: [String] -> [Pos]
readElves fieldLines = catMaybes [
                         if c == '#' then Just (x, y) else Nothing
                         | (line, y) <- fieldLines `zip` [1..maxY],
                           (c, x) <- line `zip` [1..maxX]
                       ]
  where
    maxY = length fieldLines
    maxX = length (head fieldLines)

getBounds :: [Pos] -> (X, X, Y, Y)
getBounds pos = (minX, maxX, minY, maxY)
  where
    minX = fst . minimumBy (compare `on` fst) $ pos
    maxX = fst . maximumBy (compare `on` fst) $ pos
    minY = snd . minimumBy (compare `on` snd) $ pos
    maxY = snd . maximumBy (compare `on` snd) $ pos

showElves :: [Pos] -> String
showElves pos = unlines xs
  where
    xs = [[if S.member (x, y) posSet then '#' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]]
    posSet = S.fromList pos
    (minX, maxX, minY, maxY) = getBounds pos

getNumTiles :: [Pos] -> Int
getNumTiles pos = w * h - numElves
  where
    w = maxX - minX + 1
    h = maxY - minY + 1
    numElves = length pos
    (minX, maxX, minY, maxY) = getBounds pos

getAdjacentPos :: Direction -> Pos -> Pos
getAdjacentPos N (x, y) = (x, y - 1)
getAdjacentPos S (x, y) = (x, y + 1)
getAdjacentPos E (x, y) = (x + 1, y)
getAdjacentPos W (x, y) = (x - 1, y)
getAdjacentPos NE (x, y) = (x + 1, y - 1)
getAdjacentPos NW (x, y) = (x - 1, y - 1)
getAdjacentPos SE (x, y) = (x + 1, y + 1)
getAdjacentPos SW (x, y) = (x - 1, y + 1)

isElfInDirection :: ElvesSet -> Pos -> Direction -> Bool
isElfInDirection elves pos direction = S.member pos' elves
  where
    pos' = getAdjacentPos direction pos

isElfInDirections :: ElvesSet -> Pos -> [Direction] -> Bool
isElfInDirections elves pos directions = any (\d -> isElfInDirection elves pos d) directions

isElfInAdjacentPos :: ElvesSet -> Pos -> Bool
isElfInAdjacentPos elves pos = isElfInDirections elves pos allDirections

initialProposalDirections :: [Direction]
initialProposalDirections = [N, S, W, E]

shiftDirections :: [Direction] -> [Direction]
shiftDirections [] = []
shiftDirections (x:xs) = xs ++ [x]

getRelevantDirectionsForProposal :: Direction -> [Direction]
getRelevantDirectionsForProposal N = [N, NE, NW]
getRelevantDirectionsForProposal S = [S, SE, SW]
getRelevantDirectionsForProposal W = [W, NW, SW]
getRelevantDirectionsForProposal E = [E, NE, SE]
getRelevantDirectionsForProposal d = error $ "Invalid direction for proposal: " <> show d

proposeMove :: ElvesSet -> [Direction] -> Pos -> Maybe Direction
proposeMove elves directions elfPos =
  if not (isElfInAdjacentPos elves elfPos) then Nothing
  else find (\d -> not . isElfInDirections elves elfPos $ getRelevantDirectionsForProposal d) directions

applyProposedMove :: Pos -> Maybe Direction -> Maybe Pos
applyProposedMove _ Nothing = Nothing
applyProposedMove pos (Just d) = Just $ getAdjacentPos d pos

takeTurn :: [Pos] -> [Direction] -> ([Pos], [Direction], Bool)
takeTurn elves directions =
  let elvesSet = S.fromList elves
      proposedDirections = map (\e -> proposeMove elvesSet directions e) elves
      proposedPositions = zipWith applyProposedMove elves proposedDirections
      proposedPositionCounts = histogram (catMaybes proposedPositions)
      invalidPositions = M.keysSet . M.filter (> 1) $ proposedPositionCounts
      acceptedPositions = flip map (elves `zip` proposedPositions) $ \case
        (e, Nothing) -> e
        (e, Just p) -> if S.member p invalidPositions then e
                       else p
      directions' = shiftDirections directions
      isFinished = all isNothing proposedPositions
  in (acceptedPositions, directions', isFinished)

takeTurns :: [Pos] -> [Direction] -> Int -> IO (Int, [Pos])
takeTurns elves directions round = do
  let (elves', directions', isFinished) = takeTurn elves directions
  print round
  if isFinished then return (round, elves')
  else takeTurns elves' directions' (round+1)

day23 :: IO ()
day23 = do
  -- let input = testInput
  input <- lines <$> readFile "input/Day23.txt"

  putStrLn "day23"

  let elves = readElves input
  putStrLn $ showElves elves

  let dirs = initialProposalDirections

  -- part 1
  let (elves', dirs', isFinished) = foldl (\(es, ds, _) _ -> takeTurn es ds) (elves, dirs, False) [1..10]
  putStrLn $ showElves elves'
  print $ getNumTiles elves'
  print isFinished

  -- part 2
  (maxRound, elves'') <- takeTurns elves dirs 1
  print maxRound
  print $ showElves elves''

  return ()
