{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16 where

import Control.Monad (forM_, void, when)
import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec hiding (label)
import Text.Parsec.String
import Text.RawString.QQ

import Util (lstrip, regularParse, windows)

testInput :: String
testInput = lstrip [r|
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
|]

type Label = String
type FlowRate = Int
data Valve = Valve Label FlowRate [Label]
             deriving (Eq, Show)

getValveLabel :: Valve -> Label
getValveLabel (Valve l _ _) = l

getValveFlowRate :: Valve -> Int
getValveFlowRate (Valve _ fr _) = fr

getReachableValves :: Valve -> [Label]
getReachableValves (Valve _ _ toValves) = toValves

hasZeroFlowRate :: Valve -> Bool
hasZeroFlowRate (Valve _ 0 _) = True
hasZeroFlowRate _ = False

hasNonZeroFlowRate :: Valve -> Bool
hasNonZeroFlowRate = not . hasZeroFlowRate

labelParser :: Parser Label
labelParser = do
  a <- upper
  b <- upper
  return [a, b]

valveParser :: Parser Valve
valveParser = do
  void $ string "Valve "
  label <- labelParser
  void $ string " has flow rate="
  flowRate <- read <$> many1 digit
  void $ try (string "; tunnels lead to valves ") <|> try (string "; tunnel leads to valve ")
  toLabels <- sepBy1 labelParser (string ", ")
  return $ Valve label flowRate toLabels

valvesParser :: Parser [Valve]
valvesParser = endBy1 valveParser endOfLine

type Predecessors a = Map a a

bfs' :: Ord a => (a -> Set a) -> a -> [a] -> Set a -> Predecessors a -> Predecessors a
bfs' getNeighbours current toVisit visited predecessors =
  let visited' = S.insert current visited
      reachableNodes = getNeighbours current
      reachableNodesNotVisited = S.difference reachableNodes visited'
      reachableNodesNotVisitedAndNotToVisit = S.difference reachableNodesNotVisited (S.fromList toVisit)
      toVisit' = toVisit ++ S.toList reachableNodesNotVisitedAndNotToVisit
      predecessors' = foldl (\acc x -> M.insert x current acc) predecessors reachableNodesNotVisitedAndNotToVisit

  in case toVisit' of
    [] -> predecessors'
    x:xs -> bfs' getNeighbours x xs visited' predecessors'

bfs :: Ord a => (a -> Set a) -> a -> Predecessors a
bfs getNeighbours current = bfs' getNeighbours current [] S.empty M.empty

getPath'' :: Ord a => Predecessors a -> a -> [a] -> [a]
getPath'' predecessors a path =
  case M.lookup a predecessors of
    Nothing -> a:path
    Just p -> getPath'' predecessors p (a:path)

getPath' :: Ord a => Predecessors a -> a -> [a]
getPath' predecessors a = getPath'' predecessors a []

getPath :: Ord a => Map a (Predecessors a) -> a -> a -> [a]
getPath predecessorsMap from to = case M.lookup from predecessorsMap of
  Nothing -> []
  Just m -> getPath' m to

data PathAction = Visit Label
                | Open Label
                deriving (Eq, Show)

toPathActions :: [Label] -> [PathAction]
toPathActions path =
  let from = head path
      to = last path
      mid = map Visit . init . tail $ path
      path' = [Visit from, Open from] ++ mid ++ [Visit to, Open to]
  in path'

joinPathActions :: [[PathAction]] -> [PathAction]
joinPathActions [] = []
joinPathActions [xs] = xs
joinPathActions (x:xs) =
  let xs' = map (drop 2) xs
  in concat (x:xs')

getReleasedPressureForPathActions :: Map Label Valve -> Int -> Int -> Int -> [PathAction] -> Int
getReleasedPressureForPathActions valvesMap minute releasing released actions =
  let released' = released + releasing
  in if minute == 30 then released'
     else
       let (additionalReleasing, actions') =
             case actions of
               [] -> (0, [])
               (Visit _:xs) -> (0, xs)
               (Open x:xs) ->
                 case M.lookup x valvesMap of
                   Nothing -> (0, xs)
                   Just v -> (getValveFlowRate v, xs)
           releasing' = releasing + additionalReleasing
       in getReleasedPressureForPathActions valvesMap (minute+1) releasing' released' actions'

getReleasedPressureForSchedule :: Map Label Valve -> Map Label (Predecessors Label) -> [Label] -> Int
getReleasedPressureForSchedule valvesMap predecessorsMap schedule =
  let schedule' = "AA" : schedule
      pairs = map (\xs -> (head xs, head . tail $ xs)) . windows 2 $ schedule'
      subPaths = map (uncurry (getPath predecessorsMap)) pairs
      subPathActions = map toPathActions subPaths
      pathActions = joinPathActions subPathActions
      pathActions' = drop 2 pathActions
      releasedPressure = getReleasedPressureForPathActions valvesMap 1 0 0 pathActions'
  in releasedPressure

day16 :: IO ()
day16 = do
  let input = testInput
  -- input <- readFile "input/Day16.txt"

  valves <- case regularParse valvesParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  forM_ valves print
  putStrLn $ "Number of valves: " <> show (length valves)

  let valvesMap = M.fromList [(getValveLabel v, v) | v <- valves]
      valvesLabels = [getValveLabel v | v <- valves]
      nonZeroFlowRateValves = [getValveLabel v | v <- valves, hasNonZeroFlowRate v]

  putStrLn $ "Number of valves with non-zero flow rate: " <> show (length nonZeroFlowRateValves)

  let schedules = permutations nonZeroFlowRateValves
  -- putStrLn $ "Number of schedules: " <> show (length schedules)
  -- test input:  6! = 720
  -- real input: 15! = 1.3 * 10^12

  let getNeighbours v = S.fromList $ maybe [] getReachableValves (M.lookup v valvesMap)
      predecessorsMap :: Map Label (Predecessors Label) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valvesLabels

  print $ getPath predecessorsMap "AA" "CC"
  print $ getPath predecessorsMap "CC" "GG"

  -- part 1
  let maxReleasedPressure = maximum . map (getReleasedPressureForSchedule valvesMap predecessorsMap) $ schedules
  putStrLn $ "Maximum released pressure: " <> show maxReleasedPressure
