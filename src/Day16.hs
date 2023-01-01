{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16 where

import Combinatorics
import Control.Monad (forM_, void, when)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Matrix (Matrix)
import qualified Data.Matrix as MT
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec hiding (label, labels)
import Text.Parsec.String
import Text.RawString.QQ

import Util (lstrip, regularParse)
import UtilGraphSearch (Predecessors, bfs, getShortestPathLengths)

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

type Path = [Label]
type Emission = Int
type RemainingMinutes = Int
type Visited = Set Label

data FieldInfo = FieldInfo {
  distances :: Matrix Int,
  indexes :: Map Label Int,
  valves :: Map Label Valve,
  labels :: Set Label
}

search :: FieldInfo -> Visited -> RemainingMinutes -> Path -> Label -> Emission -> [(Path, Emission)]
search fieldInfo@FieldInfo { distances, indexes, valves, labels } visited remaining path current emission =
  let toVisit = S.toList $ S.difference labels visited
      expand v =
        let visited' = S.insert v visited
            d = distances MT.! (indexes M.! current, indexes M.! v)
            remaining' = remaining - (d + 1)
        in
          if remaining' > 0 then
            let flowRate = getValveFlowRate (valves M.! v)
                emission' = emission + flowRate * remaining'
            in Just (visited', remaining', v:path, v, emission')
          else Nothing
      remainings = mapMaybe expand toVisit
      paths = if null remainings then [(reverse path, emission)]
              else concatMap (\(vs, r, p, v, e) -> search fieldInfo vs r p v e) remainings
  in paths

search2 :: FieldInfo -> Visited -> [(Label, RemainingMinutes)] -> Emission -> IO [Emission]
search2 fieldInfo@FieldInfo { distances, indexes, valves, labels } visited currentHeads emission = do
  let numActiveHeads = length currentHeads
      -- TODO for more numActiveHeads than nodes to visit, variate will return an empty list
      -- this can be alleviated by not scheduling all heads
      toVisit :: [[Label]] = if numActiveHeads == 0 then []
                else variate numActiveHeads . S.toList $ S.difference labels visited
      expand :: RemainingMinutes -> Label -> Label -> Maybe (Label, RemainingMinutes, Emission)
      expand remaining from to =
        let d = distances MT.! (indexes M.! from, indexes M.! to)
            remaining' = remaining - (d + 1)
        in
          if remaining' > 0 then
            let flowRate = getValveFlowRate (valves M.! to)
                emissionPart = flowRate * remaining'
            in Just (to, remaining', emissionPart)
          else Nothing
  let expansions :: [[(Label, RemainingMinutes, Emission)]] =
        filter (not . null) $ map (\xs -> mapMaybe (\((from, remaining), to) -> expand remaining from to) (currentHeads `zip` xs)) toVisit
      expansions' :: [[(Label, RemainingMinutes)]] = map (\xs -> map (\(l, r, _) -> (l, r)) xs) expansions
      emissions :: [Emission] = map (\xs -> emission + sum (map (\(_, _, e) -> e) xs)) expansions
      visited' :: [Visited] = map (\xs -> S.union visited (S.fromList $ map (\(v, _, _) -> v) xs)) expansions
      -- TODO if one expansion cant be done for that head, then this one will be lost
  -- putStrLn $ show currentHeads <> " - " <> show (S.toList $ S.difference labels visited) <> " - " <> show toVisit <> " - " <> show expansions
  -- putStrLn $ show currentHeads <> " - " <> show (S.toList $ S.difference labels visited)
  paths <- if null expansions then return [emission]
           else concat <$> mapM (\(vs, xs, e) -> search2 fieldInfo vs xs e) (zip3 visited' expansions' emissions)
  return paths

day16 :: IO ()
day16 = do
  -- let input = testInput
  input <- readFile "input/Day16.txt"

  valves <- case regularParse valvesParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let valveMap = M.fromList [(getValveLabel v, v) | v <- valves]
      valveLabels = [getValveLabel v | v <- valves]
      valveIdxs = M.fromList $ valveLabels `zip` [1..]
      getNeighbours v = S.fromList $ maybe [] getReachableValves (M.lookup v valveMap)
      predecessorsMap :: Map Label (Predecessors Label) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valveLabels
      distances = getShortestPathLengths valveLabels predecessorsMap
      nonZeroFlowRateValveLabels = [getValveLabel v | v <- valves, hasNonZeroFlowRate v] ++ ["AA"]
      nonZeroFlowRateValveIdxs = M.fromList $ nonZeroFlowRateValveLabels `zip` [1..]
      nonZeroFlowRateDistances = MT.fromLists [[distances MT.! (valveIdxs M.! u, valveIdxs M.! v) | u <- nonZeroFlowRateValveLabels] | v <- nonZeroFlowRateValveLabels]

  putStrLn $ "valves: " <> show valves
  putStrLn $ "valveLabels: " <> show valveLabels
  putStrLn $ "valveIdxs: " <> show valveIdxs
  putStrLn $ "nonZeroFlowRateValveLabels: " <> show nonZeroFlowRateValveLabels
  putStrLn $ "nonZeroFlowRateValveIdxs: " <> show nonZeroFlowRateValveIdxs
  putStrLn $ "Number of valves with non-zero flow rate: " <> show (length nonZeroFlowRateValveLabels)
  putStrLn "distances:"
  print distances
  putStrLn "nonZeroFlowRateDistances:"
  print nonZeroFlowRateDistances

  let fieldInfo = FieldInfo {
    distances = nonZeroFlowRateDistances,
    indexes = nonZeroFlowRateValveIdxs,
    valves = valveMap,
    labels = S.fromList nonZeroFlowRateValveLabels
  }

  -- part 1
  let xs = search fieldInfo (S.singleton "AA") 30 ["AA"] "AA" 0
  print $ length xs

  let mx = maximumBy (compare `on` snd) xs
  print mx

  -- part 2
  ys <- search2 fieldInfo (S.fromList ["AA", "AA"]) [("AA", 26), ("AA", 26)] 0
  -- ys <- search2 fieldInfo (S.fromList ["AA"]) [("AA", 30)] 0
  print $ length ys

  let mx2 = maximum ys
  print mx2
