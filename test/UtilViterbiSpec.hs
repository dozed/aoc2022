{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UtilViterbiSpec where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Matrix (Matrix)
import qualified Data.Matrix as MT
import qualified Data.Set as S

import Test.Hspec

import UtilViterbi
import UtilGraphSearch (Predecessors, bfs, getShortestPathLengths)
import UtilMatrix (dropLastColumn, getLastColumn)

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

type RemainingMinutes = Int
type PPR = Int
type ViterbiInfo' = ViterbiInfo Label (RemainingMinutes, PPR)

getB' :: Matrix Int
      -> Map Label Valve
      -> ViterbiInfo'  -- ^ The info about the structure Viterbi is applied to
      -> Matrix (Label, (RemainingMinutes, PPR))  -- ^ The current matrix of tagged, previous values
      -> Timestep  -- ^ The current timestep
      -> Label  -- ^ The `from` value for which the `b` should be computed
      -> Label  -- ^ The `to` value for which the `b` should be computed
      -> (RemainingMinutes, PPR)  -- ^ The `b` for the current edge
getB' distances valves ViterbiInfo { indexes } previous timestep fromValveLabel toValveLabel =
  let fromIdx = indexes M.! fromValveLabel
      toIdx = indexes M.! toValveLabel
      toValve = valves M.! toValveLabel
      flowRate = getValveFlowRate toValve
      distance = distances MT.! (fromIdx, toIdx)
      (remainingMinutes, prevPpr) = 
        if timestep == 1 then (30, 0)
        else let (_, (r, p)) = previous MT.! (fromIdx, timestep - 1) in (r, p)
      remainingMinutes' = remainingMinutes - distance - 1
      ppr = remainingMinutes' * flowRate
  in (remainingMinutes', prevPpr + ppr)

getPathTo :: Map Label Int -> Matrix (Label, (RemainingMinutes, PPR)) -> Label -> Int -> [Label]
getPathTo _ _ x 0 = [x]
getPathTo indexes previous valve i =
  let valveIdx = indexes M.! valve
      (x, _) = previous MT.! (valveIdx, i)
  in valve : getPathTo indexes previous x (i-1)

getCandidates' :: ViterbiInfo' -- ^ The info about the structure Viterbi is applied to
               -> Matrix (Label, (RemainingMinutes, PPR))  -- ^ The current matrix of tagged, previous values
               -> Timestep  -- ^ The current timestep
               -> Label  -- ^ The value for which the candidate should be found
               -> [Label]  -- ^ The list of candidates for that value at the current timestep
getCandidates' _ _ 0 _ = ["AA"]
getCandidates' ViterbiInfo { values, indexes } previous timestep valve =
  let candidates = filter (\v -> v /= valve && v /= "AA" && fst (previous MT.! (indexes M.! v, timestep - 1)) /= "--") values
      -- the same valve cant be the previous valve
      paths = map (\v -> getPathTo indexes previous v (timestep - 1)) candidates
      -- a previous valve which contains valve on its path cant be the previous valve
      candidates' = map fst . filter (\(c, p) -> valve `notElem` p) $ (candidates `zip` paths)
  in candidates'

utilViterbiSpec :: Spec
utilViterbiSpec = do

  describe "getPathTo" $ do
    it "should get path from previous valve matrix" $ do
      let valveLabels = ["DD", "BB", "JJ"]
          valveIdxs = M.fromList $ valveLabels `zip` [1..]
          previousValves = MT.fromLists [
              [("AA", (1, 1)), ("BB", (1, 1)), ("JJ", (1, 1))],
              [("AA", (1, 1)), ("DD", (1, 1)), ("--", (1, 1))],
              [("AA", (1, 1)), ("BB", (1, 1)), ("BB", (1, 1))]
            ]

      let path1 = getPathTo valveIdxs previousValves "DD" 3
      path1 `shouldBe` ["DD", "JJ", "BB", "AA"]

      let path2 = getPathTo valveIdxs previousValves "JJ" 1
      path2 `shouldBe` ["JJ", "AA"]

  describe "getCandidates" $ do
    it "should get candidates" $ do
      let valveLabels = ["DD", "BB", "JJ"]
          valveIdxs = M.fromList $ valveLabels `zip` [1..]
          previousValves = MT.fromLists [
              [("AA", (1, 1)), ("BB", (1, 1)), ("JJ", (1, 1))],
              [("AA", (1, 1)), ("DD", (1, 1)), ("--", (1, 1))],
              [("AA", (1, 1)), ("BB", (1, 1)), ("BB", (1, 1))]
            ]

      let info = ViterbiInfo {
        values = valveLabels,
        indexes = valveIdxs,
        mkStart = "AA",
        mkEmpty = ("--", (0, 0)),
        getCandidates = getCandidates',
        getB = getB' (MT.zero 3 3) M.empty,
        isFinished = (\i m t -> undefined)
      }

      getCandidates' info previousValves 3 "JJ" `shouldBe` ["DD", "BB"]
      getCandidates' info previousValves 3 "DD" `shouldBe` ["JJ"]

  describe "viterbi" $ do
    it "should compute viterbi state matrix" $ do
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

      let info = ViterbiInfo {
        values = nonZeroFlowRateValveLabels,
        indexes = nonZeroFlowRateValveIdxs,
        mkStart = "AA",
        mkEmpty = ("--", (0, 0)),
        getCandidates = getCandidates',
        getB = getB' nonZeroFlowRateDistances valveMap,
        isFinished = \i m t -> all ((<= 0) . fst . snd) (getLastColumn m)
      }

      let previous = dropLastColumn . viterbi $ info

      let expected = MT.fromLists [[("AA", (28, 560)), ("JJ", (23, 1027))],  -- row: AA -> JJ -> DD
                                   [("AA", (27, 567)), ("DD", (24, 1064))]]  -- row: AA -> DD -> JJ

      previous `shouldBe` expected
