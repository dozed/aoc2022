module Day14Spec (day14Spec) where

import Control.Exception (evaluate)

import Test.Hspec

import Day14
import Util (regularParse)

day14Spec :: Spec
day14Spec = do

  describe "posParser" $ do
    it "should parse a Pos" $ do
      regularParse posParser "498,4" `shouldBe` Right (498, 4)

  describe "pathParser" $ do
    it "should parse a Path" $ do
      regularParse pathParser "498,4 -> 498,6 -> 496,6" `shouldBe` Right [(498, 4), (498, 6), (496, 6)]

  describe "pathsParser" $ do
    it "should parse a list of Path" $ do
      regularParse pathsParser testInput1 `shouldBe` Right [
          [(498, 4), (498, 6), (496, 6)],
          [(503, 4), (502, 4), (502, 9), (494, 9)]
        ]

  describe "getOrientation" $ do
    it "should get Horizontal orientation" $ do
      getOrientation (498, 6) (496, 6) `shouldBe` Horizontal

    it "should get Vertical orientation" $ do
      getOrientation (498, 4) (498, 6) `shouldBe` Vertical

    it "should throw for non-Horizontal and non-Vertical orientation" $ do
      evaluate (getOrientation (498, 4) (499, 5)) `shouldThrow` anyErrorCall
