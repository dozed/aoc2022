module Day16Spec where

import Test.Hspec

import Day16
import Util (regularParse)

day16Spec :: Spec
day16Spec = do
  
  describe "valveParser" $ do
    it "should parse a Valve" $ do
      regularParse valveParser "AA" `shouldBe` Right "AA"

  describe "descParser" $ do
    it "should parse a Desc" $ do
      let txt = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
      regularParse descParser txt `shouldBe` Right (Desc "AA" 0 ["DD", "II", "BB"])
