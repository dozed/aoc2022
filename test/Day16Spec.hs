{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16Spec where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Matrix as MT
import Data.Set (Set)
import qualified Data.Set as S
import Text.RawString.QQ

import Test.Hspec

import Day16 hiding (example1, example2, example3)
import Util (lstrip, regularParse)

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

  describe "getPath" $ do
    it "should compute the shorted path between two valves" $ do
      let input = testInput

      valves <- case regularParse valvesParser input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let valvesMap = M.fromList [(getValveLabel v, v) | v <- valves]
          valvesLabels = [getValveLabel v | v <- valves]
          getNeighbours a = S.fromList $ maybe [] getReachableValves (M.lookup a valvesMap)
          predecessorsMap :: Map Label (Predecessors Label) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valvesLabels

      getPath predecessorsMap "AA" "CC" `shouldBe` ["AA", "BB", "CC"]
      getPath predecessorsMap "CC" "GG" `shouldBe` ["CC", "DD", "EE", "FF", "GG"]

  describe "toPathActions" $ do
    it "should transform a sub-path to a list of PathActions" $ do
      toPathActions ["EE", "FF", "GG", "HH"] `shouldBe` [Visit "EE", Open "EE", Visit "FF", Visit "GG", Visit "HH", Open "HH"]

  describe "joinPathActions" $ do
    it "should join sub-PathActions to a single list of PathActions" $ do
      let subPathActions = [[Visit "BB", Open "BB", Visit "CC", Open "CC"], [Visit "CC", Open "CC", Visit "FF", Visit "HH", Open "HH"]]
      let pathActions = [Visit "BB", Open "BB", Visit "CC", Open "CC", Visit "FF", Visit "HH", Open "HH"]

      joinPathActions subPathActions `shouldBe` pathActions

  describe "getReleasedPressureForPathActions" $ do
    it "should compute released pressure for a given PathAction list over 30 minutes" $ do
      let input = testInput

      valves <- case regularParse valvesParser input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let valvesMap = M.fromList [(getValveLabel v, v) | v <- valves]
          pathActions = [Visit "DD", Open "DD", Visit "CC", Visit "BB", Open "BB", Visit "AA", Visit "II", Visit "JJ", Open "JJ",
                          Visit "II", Visit "AA", Visit "DD", Visit "EE", Visit "FF", Visit "GG", Visit "HH", Open "HH", Visit "GG",
                          Visit "FF", Visit "EE", Open "EE", Visit "DD", Visit "CC", Open "CC"]

      let releasedPressure = getReleasedPressureForPathActions valvesMap 1 0 0 pathActions

      releasedPressure `shouldBe` 1651

  describe "getReleasedPressureForSchedule" $ do
    it "should compute the released pressure for a given schedule" $ do
      let input = testInput

      valves <- case regularParse valvesParser input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let valvesMap = M.fromList [(getValveLabel v, v) | v <- valves]
          valvesLabels = [getValveLabel v | v <- valves]
          getNeighbours a = S.fromList $ maybe [] getReachableValves (M.lookup a valvesMap)
          predecessorsMap :: Map Label (Predecessors Label) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valvesLabels

      let schedule = ["DD", "BB", "JJ", "HH", "EE", "CC"]
      getReleasedPressureForSchedule valvesMap predecessorsMap schedule `shouldBe` 1651

  describe "toActions" $ do
    it "should compute an Action list for a given schedule" $ do
      let input = testInput

      valves <- case regularParse valvesParser input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let valvesMap = M.fromList [(getValveLabel v, v) | v <- valves]
          valvesLabels = [getValveLabel v | v <- valves]
          valvesIdxs = M.fromList $ valvesLabels `zip` [1..]
          getNeighbours a = S.fromList $ maybe [] getReachableValves (M.lookup a valvesMap)
          predecessorsMap :: Map Label (Predecessors Label) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valvesLabels
          shortestPathLengths = getShortestPathLengths valvesLabels predecessorsMap

      let schedule = ["DD", "BB"]
      let expected = [TravelTo "DD" 1, OpenIt "DD", TravelTo "BB" 2, OpenIt "BB"]
      toActions shortestPathLengths valvesIdxs schedule `shouldBe` expected

      let schedule2 = ["DD", "BB", "JJ", "HH", "EE", "CC"]
      let expected2 = [TravelTo "DD" 1,OpenIt "DD",TravelTo "BB" 2,OpenIt "BB",TravelTo "JJ" 3,OpenIt "JJ",TravelTo "HH" 7,OpenIt "HH",TravelTo "EE" 3,OpenIt "EE",TravelTo "CC" 2,OpenIt "CC"]
      toActions shortestPathLengths valvesIdxs schedule2 `shouldBe` expected2

  describe "getReleasedPressureForPathActions'" $ do
    it "should compute the released pressure for a given Action list" $ do
      let input = testInput

      valves <- case regularParse valvesParser input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let valvesMap = M.fromList [(getValveLabel v, v) | v <- valves]
          valvesLabels = [getValveLabel v | v <- valves]
          valvesIdxs = M.fromList $ valvesLabels `zip` [1..]
          getNeighbours a = S.fromList $ maybe [] getReachableValves (M.lookup a valvesMap)
          predecessorsMap :: Map Label (Predecessors Label) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valvesLabels
          shortestPathLengths = getShortestPathLengths valvesLabels predecessorsMap

      let actions = toActions shortestPathLengths valvesIdxs ["DD", "BB", "JJ", "HH", "EE", "CC"]
      getReleasedPressureForPathActions' valvesMap 1 0 0 actions `shouldBe` 1651

  -- Viterbi
  describe "getPathTo" $ do
    it "should get path from previous valve matrix" $ do
      let valveLabels = ["DD", "BB", "JJ"]
          valveIdxs = M.fromList $ valveLabels `zip` [1..]
          previousValves = MT.fromLists [
              ["AA", "BB", "JJ"],
              ["AA", "DD", "--"],
              ["AA", "BB", "BB"]
            ]

      let path1 = getPathTo previousValves valveIdxs "DD" 3
      path1 `shouldBe` ["DD", "JJ", "BB", "AA"]

      let path2 = getPathTo previousValves valveIdxs "JJ" 1
      path2 `shouldBe` ["JJ", "AA"]

  describe "getCandidates" $ do
    it "should get candidates" $ do
      let valveLabels = ["DD", "BB", "JJ"]
          valveIdxs = M.fromList $ valveLabels `zip` [1..]
          previousValves = MT.fromLists [
              ["AA", "BB", "JJ"],
              ["AA", "DD", "--"],
              ["AA", "BB", "BB"]
            ]

      getCandidates valveLabels valveIdxs previousValves 3 "JJ" `shouldBe` ["DD", "BB"]
      getCandidates valveLabels valveIdxs previousValves 3 "DD" `shouldBe` ["JJ"]

  describe "appendColumn" $ do
    it "should append a column with a default value to a matrix" $ do
      let m = MT.fromLists [[2], [3]]

      appendColumn 0 m `shouldBe` MT.fromLists [[2, 0], [3, 0]]

  describe "getLastColumn" $ do
    it "should return the last column of a matrix" $ do
      let m = MT.fromLists [[1, 2], [3, 4]]

      getLastColumn m `shouldBe` [2, 4]
