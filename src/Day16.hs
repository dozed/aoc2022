{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16 where

import Control.Monad (forM_, void)
import Data.Function (on)
import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (maximumBy)
import Data.Matrix (Matrix)
import qualified Data.Matrix as MT
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace)
import Text.Parsec hiding (label)
import Text.Parsec.String
import Text.RawString.QQ

import Util (findElem, lstrip, regularParse, windows)

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

getShortestPathLengths :: Ord a => [a] -> Map a (Predecessors a) -> Matrix Int
getShortestPathLengths valveLabels predecessorsMap =
  let xxs = [[length (getPath predecessorsMap x y) - 1 | x <- valveLabels] | y <- valveLabels]
      matrix = MT.fromLists xxs
  in matrix

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
               ((Visit _):xs) -> (0, xs)
               ((Open l):xs) ->
                 case M.lookup l valvesMap of
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

data Action = OpenIt Label
            | TravelTo Label Int
            deriving (Eq, Show)

toActions :: Matrix Int -> Map Label Int -> [Label] -> [Action]
toActions pathLengths valvesIdx schedule =
  let schedule' :: [Label] = "AA" : schedule
      pairs = map (\xs -> (head xs, head . tail $ xs)) . windows 2 $ schedule'
      getPathLength f t = MT.getElem (valvesIdx M.! f) (valvesIdx M.! t) pathLengths
      actions = concatMap (\(f, t) -> [TravelTo t (getPathLength f t), OpenIt t]) pairs
  in actions

getReleasedPressureForPathActions' :: Map Label Valve -> Int -> Int -> Int -> [Action] -> Int
getReleasedPressureForPathActions' valvesMap minute releasing released actions =
  let released' = released + releasing
  in if minute == 30 then released'
     else
       let (additionalReleasing, actions') =
             case actions of
               [] -> (0, [])
               ((TravelTo _ 1):xs) -> (0, xs)
               ((TravelTo v n):xs) -> (0, TravelTo v (n-1):xs)
               ((OpenIt l):xs) ->
                 case M.lookup l valvesMap of
                   Nothing -> (0, xs)
                   Just v -> (getValveFlowRate v, xs)
           releasing' = releasing + additionalReleasing
       in getReleasedPressureForPathActions' valvesMap (minute+1) releasing' released' actions'

getReleasedPressureForSchedule' :: Map Label Valve -> Matrix Int -> Map Label Int -> [Label] -> Int
getReleasedPressureForSchedule' valvesMap shortestPathLengths valvesIdxs schedule =
  let actions = toActions shortestPathLengths valvesIdxs schedule
  in getReleasedPressureForPathActions' valvesMap 1 0 0 actions

type AdjacencyMap a = Map a [a]
type AdjacencyMapWithDistance a = Map a [(a, Int)]

getAdjacencyMap :: [Valve] -> AdjacencyMap Label
getAdjacencyMap valves = M.fromList $ map (\v -> (getValveLabel v, getReachableValves v)) valves

maybeInsertEdge :: (Label, Label, Int) -> AdjacencyMapWithDistance Label -> AdjacencyMapWithDistance Label
maybeInsertEdge (from, to, distance) adjMap =
  case M.lookup from adjMap of
    Nothing -> M.insert from [(to, distance)] adjMap
    Just xs -> case findElem (\(x, _) -> x == to) xs of
      Nothing -> M.insert from ((to, distance):xs) adjMap
      Just ((_, oldDistance), xs') ->
        if distance < oldDistance then M.insert from ((to, distance):xs') adjMap
        else adjMap

removeNode :: Label -> AdjacencyMapWithDistance Label -> AdjacencyMapWithDistance Label
removeNode valveLabel adjacencyMap =
  let adjacentNodes = adjacencyMap M.! valveLabel
      -- delete node from adjacencyMap
      adjacencyMap' = M.delete valveLabel adjacencyMap
      -- delete entries to valveLabel in adjacencyMap
      adjacencyMap'' = M.map (filter (\(x, _) -> x /= valveLabel)) adjacencyMap'
      -- generate new edges between all adjacentNodes
      xs :: [(Label, Label, Int)] = [(v1, v2, d1 + d2) | (v1, d1) <- adjacentNodes, (v2, d2) <- adjacentNodes, v1 /= v2]
      -- maybe add new edges with new distance to adjacencyMap
      adjacencyMap''' = foldl (flip maybeInsertEdge) adjacencyMap'' xs
  in adjacencyMap'''

initAdjacencyMapWithDistances :: AdjacencyMap a -> AdjacencyMapWithDistance a
initAdjacencyMapWithDistances am = M.map (\xs -> map (\x -> (x, 1)) xs) am

type PPR = Int
type RemainingMinutes = Int

getPotentialPressureRelease :: Matrix Int -> Map Label Int -> Map Label Valve -> Int -> Label -> Label -> (PPR, RemainingMinutes)
getPotentialPressureRelease distances valvesIdx valvesMap remainingMinutes from to =
  let fromIdx = valvesIdx M.! from
      toIdx = valvesIdx M.! to
      toValve = valvesMap M.! to
      flowRate = getValveFlowRate toValve
      distance = distances MT.! (fromIdx, toIdx)
      remainingMinutes' = remainingMinutes - distance - 1
      ppr = remainingMinutes' * flowRate
  in (ppr, remainingMinutes')

getPotentialPressureRelease' :: Matrix Int -> Map Label Int -> Map Label Valve -> Int -> Label -> Label -> IO Int
getPotentialPressureRelease' distances valvesIdx valvesMap remainingMinutes from to = do
  let fromIdx = valvesIdx M.! from
      toIdx = valvesIdx M.! to
      toValve = valvesMap M.! to
      flowRate = getValveFlowRate toValve
      distance = distances MT.! (fromIdx, toIdx)
      ppr = (remainingMinutes - distance - 1) * flowRate
  putStrLn $ "r: " <> show remainingMinutes <> " d: " <> show distance <> " f: " <> show flowRate <> " ppr: " <> show ppr
  return ppr

getPathTo :: Matrix Label -> Map Label Int -> Label -> Int -> [Label]
getPathTo _ _ x 0 = [x]
getPathTo previousValves valveIdxs valve i =
  let valveIdx = valveIdxs M.! valve
      x = previousValves MT.! (valveIdx, i)
  in valve : getPathTo previousValves valveIdxs x (i-1)

getCandidates :: [Label] -> Map Label Int -> Matrix Label -> Int -> Label -> [Label]
getCandidates _ _ _ 0 _ = ["AA"]
getCandidates valveLabels valveIdxs previousValves pathLength valve =
  let candidates = filter (\v -> v /= valve && v /= "AA") valveLabels  -- the same valve cant be the previous valve
      paths = map (\v -> getPathTo previousValves valveIdxs v (pathLength - 1)) candidates
      -- a previous valve which contains valve on its path cant be the previous valve
      candidates' = map fst . filter (\(c, p) -> valve `notElem` p) $ (candidates `zip` paths)
  in candidates'

viterbiStepForValve :: Matrix Int -> [Label] -> Map Label Int -> Map Label Valve -> Matrix Label -> Matrix Int -> Matrix Int -> Int -> Label -> (Matrix Label, Matrix Int, Matrix Int)
viterbiStepForValve distances valveLabels valveIdxs valveMap previousValves pprs remainingMinutes timestep@1 toValve =
  let valveIdx = valveIdxs M.! toValve
      (maxLabel, (maxPPR, maxRemaining)) = ("AA", getPotentialPressureRelease distances valveIdxs valveMap 30 "AA" toValve)
      previousValves' = MT.setElem maxLabel (valveIdx, timestep) previousValves
      remainingMinutes' = MT.setElem maxRemaining (valveIdx, timestep) remainingMinutes
      pprs' = MT.setElem maxPPR (valveIdx, timestep) pprs
  in (previousValves', remainingMinutes', pprs')
viterbiStepForValve distances valveLabels valveIdxs valveMap previousValves pprs remainingMinutes timestep toValve =
  -- some valves dont have previous values -> paths ends
  -- have a matrix of tuples instead of three matrices
  let valveIdx = valveIdxs M.! toValve
      candidates = getCandidates valveLabels valveIdxs previousValves timestep toValve
      candidatesIdxs = map (valveIdxs M.!) candidates
      remainings = map (\ci -> remainingMinutes MT.! (ci, timestep - 1)) candidatesIdxs
      getAccPPRAndRemaining fromValve fromRemaining =
        let (curPpr, currentRemaining) = getPotentialPressureRelease distances valveIdxs valveMap fromRemaining fromValve toValve
            prevPpr = pprs MT.! (valveIdxs M.! fromValve, timestep - 1)
        in (prevPpr + curPpr, currentRemaining)
      pprsRemainings = map (uncurry getAccPPRAndRemaining) (candidates `zip` remainings)
      (maxLabel, (maxPPR, maxRemaining)) = maximumBy (compare `on` (fst . snd)) (candidates `zip` pprsRemainings)
      -- map of previous valve at a given timestep for a given valve
      previousValves' = MT.setElem maxLabel (valveIdx, timestep) previousValves
      -- keep a matrix of remaining minutes
      remainingMinutes' = MT.setElem maxRemaining (valveIdx, timestep) remainingMinutes
      pprs' = MT.setElem maxPPR (valveIdx, timestep) pprs
  in (previousValves', remainingMinutes', pprs')

example1 :: IO ()
example1 = do
  putStrLn "example1"

  let valves = [
          Valve "AA" 0 ["DD","II"],
          Valve "DD" 20 ["AA"],
          Valve "II" 0 ["AA","JJ"],
          Valve "JJ" 21 ["II"]
        ]

  let valveMap = M.fromList [(getValveLabel v, v) | v <- valves]
      valveLabels = [getValveLabel v | v <- valves]
      valveIdxs = M.fromList $ valveLabels `zip` [1..]
      nonZeroFlowRateValveLabels = [getValveLabel v | v <- valves, hasNonZeroFlowRate v] ++ ["AA"]
      nonZeroFlowRateValveIdxs = M.fromList $ nonZeroFlowRateValveLabels `zip` [1..]
      getNeighbours v = S.fromList $ maybe [] getReachableValves (M.lookup v valveMap)
      predecessorsMap :: Map Label (Predecessors Label) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valveLabels
      distances = getShortestPathLengths valveLabels predecessorsMap
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

  print $ getPath predecessorsMap "DD" "JJ"

  void $ getPotentialPressureRelease' nonZeroFlowRateDistances nonZeroFlowRateValveIdxs valveMap 30 "AA" "DD"
  void $ getPotentialPressureRelease' nonZeroFlowRateDistances nonZeroFlowRateValveIdxs valveMap 30 "AA" "JJ"

  let previousValves =
        MT.fromLists [
          ["--", "--"],
          ["--", "--"]
        ]

  let remainingMinutes =
        MT.fromLists  [
          [0, 0],
          [0, 0]
        ]

  let pprs =
        MT.fromLists  [
          [0, 0],
          [0, 0]
        ]

  let (previousValves1, remainingMinutes1, pprs1) =
        viterbiStepForValve nonZeroFlowRateDistances nonZeroFlowRateValveLabels nonZeroFlowRateValveIdxs valveMap previousValves pprs remainingMinutes 1 "DD"
  print "step1.1"
  print previousValves1
  print remainingMinutes1
  print pprs1

  let (previousValves2, remainingMinutes2, pprs2) =
        viterbiStepForValve nonZeroFlowRateDistances nonZeroFlowRateValveLabels nonZeroFlowRateValveIdxs valveMap previousValves1 pprs1 remainingMinutes1 1 "JJ"
  print "step1.2"
  print previousValves2
  print remainingMinutes2
  print pprs2

--  print $ getPathTo previousValves2 nonZeroFlowRateValveIdxs "DD" 1
--  print $ getPathTo previousValves2 nonZeroFlowRateValveIdxs "AA" 1

  let (previousValves3, remainingMinutes3, pprs3) =
        viterbiStepForValve nonZeroFlowRateDistances nonZeroFlowRateValveLabels nonZeroFlowRateValveIdxs valveMap previousValves2 pprs2 remainingMinutes2 2 "DD"
  print "step2.1"
  print previousValves3
  -- TODO remainingMinutes3 is not correct
  print remainingMinutes3
  print pprs3

  let (previousValves4, remainingMinutes4, pprs4) =
        viterbiStepForValve nonZeroFlowRateDistances nonZeroFlowRateValveLabels nonZeroFlowRateValveIdxs valveMap previousValves3 pprs3 remainingMinutes3 2 "JJ"
  print "step2.2"
  print previousValves4
  print remainingMinutes4
  print pprs4

  -- ┌           ┐
  -- │ "AA" "JJ" │
  -- │ "AA" "DD" │
  -- └           ┘
  -- ┌       ┐
  -- │ 28 23 │
  -- │ 27 24 │
  -- └       ┘
  -- ┌           ┐
  -- │  560 1027 │
  -- │  567 1064 │
  -- └           ┘

  pure ()


day16 :: IO ()
day16 = do
  let input = testInput
  -- input <- readFile "input/Day16.txt"

  valves <- case regularParse valvesParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

--  forM_ valves print
--  putStrLn $ "Number of valves: " <> show (length valves)

--  let valves = [
--          Valve "AA" 0 ["DD","BB","JJ"],
--          Valve "BB" 13 ["AA"],
--          Valve "DD" 20 ["AA"],
--          -- Valve "II" 0 ["AA","JJ"],
--          Valve "JJ" 21 ["AA"]
--        ]

--  let valves = [
--          Valve "AA" 0 ["DD","II"],
--          Valve "DD" 20 ["AA"],
--          Valve "II" 0 ["AA","JJ"],
--          Valve "JJ" 21 ["II"]
--        ]

--  let valves = [
--          Valve "AA" 0 ["DD","BB","II"],
--          Valve "DD" 20 ["AA"],
--          Valve "BB" 13 ["AA"],
--          Valve "II" 0 ["AA","JJ"],
--          Valve "JJ" 21 ["II"]
--        ]
--
--  let valveMap = M.fromList [(getValveLabel v, v) | v <- valves]
--      valveLabels = [getValveLabel v | v <- valves]
--      valveIdxs = M.fromList $ valveLabels `zip` [1..]
--      nonZeroFlowRateValveLabels = [getValveLabel v | v <- valves, hasNonZeroFlowRate v]
--      nonZeroFlowRateValveIdxs = M.fromList $ nonZeroFlowRateValveLabels `zip` [1..]
--      getNeighbours v = S.fromList $ maybe [] getReachableValves (M.lookup v valveMap)
--      predecessorsMap :: Map Label (Predecessors Label) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valveLabels
--      distances = getShortestPathLengths valveLabels predecessorsMap
--
--  putStrLn $ "valves: " <> show valves
--  putStrLn $ "valvesIdx: " <> show valveIdxs
--  putStrLn $ "nonZeroFlowRateValves: " <> show nonZeroFlowRateValveLabels
--  putStrLn $ "Number of valves with non-zero flow rate: " <> show (length nonZeroFlowRateValveLabels)
--
--  void $ getPotentialPressureRelease' distances valveIdxs valveMap 30 "AA" "DD"
--  void $ getPotentialPressureRelease' distances valveIdxs valveMap 30 "AA" "JJ"
--
--  let previousValves =
--        MT.fromLists [
--          ["AA", "BB", "JJ"],
--          ["AA", "DD", "--"],
--          ["AA", "BB", "BB"]
--        ]
--
--  let remainingTimes =
--        MT.fromLists  [
--          [30, 0, 0],
--          [30, 0, 0],
--          [30, 0, 0]
--        ]
--
--  let pprs =
--        MT.fromLists  [
--          [0, 0, 0],
--          [0, 0, 0],
--          [0, 0, 0]
--        ]
--
--  let valveIdx = valveIdxs M.! "DD"
--      x = previousValves MT.! (valveIdx, 3)
--
--  putStrLn $ "valveIdx: " <> show valveIdx
--  putStrLn $ "x: " <> show x
--
--  let path = getPathTo previousValves nonZeroFlowRateValveIdxs "DD" 3
--  print path

  example1

  -- startValve <- case M.lookup "AA" valvesMap of
  --   Nothing -> fail "Could not find start valve"
  --   Just v -> pure v

--   potential pressure release strategy
--  let pprs = map (\n -> (n, getPotentialPressureRelease distances valvesIdx valvesMap 30 "AA" n)) valvesLabels
--  print pprs
--
--  putStrLn "1:"
--  void $ getPotentialPressureRelease' distances valvesIdx valvesMap 30 "AA" "JJ"
--  void $ getPotentialPressureRelease' distances valvesIdx valvesMap 30 "AA" "DD"
--
--  putStrLn "1:"
--  forM_ (map (getPotentialPressureRelease distances valvesIdx valvesMap 30 "AA") valvesLabels `zip` valvesLabels) $ \(ppr, n) -> do
--    putStrLn $ n <> " - " <> show ppr <> " - " <> show (distances MT.! (valvesIdx M.! "AA", valvesIdx M.! n))
--
--  putStrLn "2:"
--  forM_ (map (getPotentialPressureRelease distances valvesIdx valvesMap 27 "JJ") valvesLabels `zip` valvesLabels) $ \(ppr, n) -> do
--    putStrLn $ n <> " - " <> show ppr <> " - " <> show (distances MT.! (valvesIdx M.! "JJ", valvesIdx M.! n))
--
--  putStrLn "3:"
--  forM_ (map (getPotentialPressureRelease distances valvesIdx valvesMap 23 "DD") valvesLabels `zip` valvesLabels) $ \(ppr, n) -> do
--    putStrLn $ n <> " - " <> show ppr <> " - " <> show (distances MT.! (valvesIdx M.! "DD", valvesIdx M.! n))
--
--  putStrLn "4:"
--  forM_ (map (getPotentialPressureRelease distances valvesIdx valvesMap 18 "HH") valvesLabels `zip` valvesLabels) $ \(ppr, n) -> do
--    putStrLn $ n <> " - " <> show ppr <> " - " <> show (distances MT.! (valvesIdx M.! "HH", valvesIdx M.! n))
--
--  putStrLn "5:"
--  forM_ (map (getPotentialPressureRelease distances valvesIdx valvesMap 14 "EE") valvesLabels `zip` valvesLabels) $ \(ppr, n) -> do
--    putStrLn $ n <> " - " <> show ppr <> " - " <> show (distances MT.! (valvesIdx M.! "EE", valvesIdx M.! n))
--
--  putStrLn "6:"
--  forM_ (map (getPotentialPressureRelease distances valvesIdx valvesMap 10 "BB") valvesLabels `zip` valvesLabels) $ \(ppr, n) -> do
--    putStrLn $ n <> " - " <> show ppr <> " - " <> show (distances MT.! (valvesIdx M.! "BB", valvesIdx M.! n))

  -- graph distances
  --  let adjacencyMap = getAdjacencyMap valves
  --      adjacencyMapWithDistances = initAdjacencyMapWithDistances adjacencyMap
  --      adjacencyMapWithDistances' = removeNode "BB" adjacencyMapWithDistances
  --
  --  print adjacencyMap
  --  print adjacencyMapWithDistances
  --  print adjacencyMapWithDistances'
  --
  --  let actions = toActions shortestPathLengths valvesIdxs ["DD", "BB", "JJ", "HH", "EE", "CC"]
  --  print $ getReleasedPressureForPathActions' valvesMap 1 0 0 actions
  --
  --  let actions = toActions shortestPathLengths valvesIdxs ["JJ", "BB", "DD", "HH", "EE", "CC"]
  --  print $ getReleasedPressureForPathActions' valvesMap 1 0 0 actions
  --
  --  let actions = toActions shortestPathLengths valvesIdxs ["JJ", "DD", "BB", "HH", "EE", "CC"]
  --  print $ getReleasedPressureForPathActions' valvesMap 1 0 0 actions

--  let schedules = permutations nonZeroFlowRateValves
  -- putStrLn $ "Number of schedules: " <> show (length schedules)
  -- test input:  6! = 720
  -- real input: 15! = 1.3 * 10^12

  --  print $ getPath predecessorsMap "AA" "CC"
  --  print $ getPath predecessorsMap "CC" "GG"
  --  print valvesLabels
  --  print shortestPathLengths
  --  print $ getPath predecessorsMap "AA" "AA"
  --  print $ getPath predecessorsMap "AA" "BB"
  --  print $ getPath predecessorsMap "AA" "DD"
  --  print $ getPath predecessorsMap "DD" "BB"
  --  print $ toActions shortestPathLengths valvesIdxs ["DD", "BB", "JJ", "HH", "EE", "CC"]
  --  let actions = toActions shortestPathLengths valvesIdxs ["DD", "BB", "JJ", "HH", "EE", "CC"]
  --  print $ getReleasedPressureForPathActions' valvesMap 1 0 0 actions

--  forM_ schedules $ \perm -> do
--    -- print perm
--    return ()

  -- part 1
--  forM_ (schedules `zip` [1..]) $ \(s, i) -> do
--    let rel = getReleasedPressureForSchedule' valveMap distances valveIdxs s
--    putStrLn $ show i <> ": " <> show rel <> " - " <> show s

  --  let maxReleasedPressure = maximum . map (getReleasedPressureForSchedule' valvesMap shortestPathLengths valvesIdxs) $ schedules
  --  putStrLn $ "Maximum released pressure: " <> show maxReleasedPressure
