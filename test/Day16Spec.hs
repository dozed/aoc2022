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

