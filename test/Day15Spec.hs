{-# LANGUAGE QuasiQuotes #-}

module Day15Spec (day15Spec) where

import Text.RawString.QQ

import Test.Hspec

import Day15
import Util (lstrip, regularParse)

testInput1 :: String
testInput1 = lstrip [r|
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
|]


day15Spec :: Spec
day15Spec = do

  describe "lineParser" $ do
    it "should parse an Info" $ do
      let txt = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
      regularParse infoParser txt `shouldBe` Right (Info (2, 18) (-2, 15))

  describe "linesParser" $ do
    it "should parse a list of Info" $ do
      regularParse infosParser testInput1 `shouldBe` Right [Info (2, 18) (-2, 15), Info (9, 16) (10, 16)]

  describe "getManhattanDistance" $ do
    it "should compute the manhattan distance for two Pos values" $ do
      let p1 = (8, 7)
          p2 = (2, 10)

      getManhattanDistance p1 p2 `shouldBe` 9

  describe "getCoveredRowPoints" $ do
    it "should compute covered points in the current row in a specific distance" $ do
      let p = (8, 0)
          n = 2

      getCoveredRowPoints p n `shouldBe` [(6, 0), (7, 0), (8, 0), (9, 0), (10, 0)]
