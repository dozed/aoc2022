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
