{-# LANGUAGE ScopedTypeVariables #-}

module UtilGraphSearchSpec where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

import Test.Hspec

import UtilGraphSearch

utilGraphSearchSpec :: Spec
utilGraphSearchSpec = do

  describe "getPath" $ do
    it "should compute the shorted path between two valves" $ do
      let valvesMap = M.fromList [
              ("AA", ["DD", "II", "BB"]),
              ("BB", ["CC", "AA"]),
              ("CC", ["DD", "BB"]),
              ("DD", ["CC", "AA", "EE"]),
              ("EE", ["FF", "DD"]),
              ("FF", ["EE", "GG"]),
              ("GG", ["FF", "HH"]),
              ("HH", ["GG"]),
              ("II", ["AA", "JJ"]),
              ("JJ", ["II"])
            ]

          valvesLabels = M.keys valvesMap
          getNeighbours a = S.fromList $ fromMaybe [] (M.lookup a valvesMap)
          predecessorsMap :: Map String (Predecessors String) = foldl (\acc v -> M.insert v (bfs getNeighbours v) acc) M.empty valvesLabels

      getPath predecessorsMap "AA" "CC" `shouldBe` ["AA", "BB", "CC"]
      getPath predecessorsMap "CC" "GG" `shouldBe` ["CC", "DD", "EE", "FF", "GG"]

