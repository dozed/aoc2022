{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16Spec where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.RawString.QQ

import Test.Hspec

import Day16
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

  describe "getReleasedPressure" $ do
    it "should compute released pressure for a given PathAction list over 30 minutes" $ do
      let input = testInput

      valves <- case regularParse valvesParser input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let valvesMap = M.fromList [(getValveLabel v, v) | v <- valves]
          pathActions = [Visit "DD", Open "DD", Visit "CC", Visit "BB", Open "BB", Visit "AA", Visit "II", Visit "JJ", Open "JJ",
                          Visit "II", Visit "AA", Visit "DD", Visit "EE", Visit "FF", Visit "GG", Visit "HH", Open "HH", Visit "GG",
                          Visit "FF", Visit "EE", Open "EE", Visit "DD", Visit "CC", Open "CC"]

      let releasedPressure = getReleasedPressure valvesMap 1 0 0 pathActions

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
