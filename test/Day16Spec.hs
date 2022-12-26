{-# LANGUAGE QuasiQuotes #-}

module Day16Spec where

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
|]

day16Spec :: Spec
day16Spec = do

  describe "valveParser" $ do
    it "should parse a Valve" $ do
      regularParse valveParser "AA" `shouldBe` Right "AA"

  describe "descParser" $ do
    it "should parse a Desc" $ do
      regularParse descParser example1 `shouldBe` Right (Desc "AA" 0 ["DD", "II", "BB"])

  describe "descsParser" $ do
    it "should parse a list of Descs" $ do
      regularParse descsParser example2 `shouldBe` Right [Desc "AA" 0 ["DD", "II", "BB"], Desc "BB" 13 ["CC", "AA"]]
