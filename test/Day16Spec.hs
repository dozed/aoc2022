{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16Spec (day16Spec) where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Matrix as MT
import qualified Data.Set as S
import Text.RawString.QQ

import Test.Hspec

import Day16
import Util (lstrip, regularParse)
import UtilGraphSearch (Predecessors, bfs, getShortestPathLengths)

example1 :: String
example1 = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"

example2 :: String
example2 = lstrip [r|
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve HH has flow rate=22; tunnel leads to valve GG
|]

day16Spec :: Spec
day16Spec = do

  describe "labelParser" $ do
    it "should parse a Valve" $ do
      regularParse labelParser "AA" `shouldBe` Right "AA"

  describe "valveParser" $ do
    it "should parse a Valve" $ do
      regularParse valveParser example1 `shouldBe` Right (Valve "AA" 0 ["DD", "II", "BB"])

  describe "valvesParser" $ do
    it "should parse a list of Valves" $ do
      let expected = [Valve "AA" 0 ["DD", "II", "BB"], Valve "BB" 13 ["CC", "AA"], Valve "HH" 22 ["GG"]]
      regularParse valvesParser example2 `shouldBe` Right expected

  describe "search" $ do
    it "should find the path with the maximum releasable pressure" $ do
      let input = testInput

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

      let fieldInfo = FieldInfo {
        distances = nonZeroFlowRateDistances,
        indexes = nonZeroFlowRateValveIdxs,
        valves = valveMap,
        labels = S.fromList nonZeroFlowRateValveLabels
      }

      let xs = search fieldInfo (S.singleton "AA") 30 ["AA"] "AA" 0
      length xs `shouldBe` 720

      let (maxPath, maxReleasablePressure) = maximumBy (compare `on` snd) xs
      maxPath `shouldBe` ["AA","DD","BB","JJ","HH","EE","CC"]
      maxReleasablePressure `shouldBe` 1651
