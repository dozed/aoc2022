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

  describe "mkPathSegment" $ do
    it "should make PathSegment" $ do
      let from = (1, 1)
      let to = (10, 1)
      mkPathSegment from to `shouldBe` (from, to)

  describe "getOrientation" $ do
    it "should get Horizontal orientation" $ do
      getOrientation (mkPathSegment (498, 6) (496, 6)) `shouldBe` Horizontal

    it "should get Vertical orientation" $ do
      getOrientation (mkPathSegment (498, 4) (498, 6)) `shouldBe` Vertical

    it "should throw for non-Horizontal and non-Vertical orientation" $ do
      evaluate (getOrientation (mkPathSegment (498, 4) (499, 5))) `shouldThrow` anyErrorCall

  describe "getRange" $ do
    it "should compute increasing range" $ do
      getRange 1 3 `shouldBe` [1, 2, 3]

    it "should compute decreasing range" $ do
      getRange 3 1 `shouldBe` [3, 2, 1]

  describe "expandPathSegment" $ do
    it "should expand a horizontal PathSegment" $ do
      let seg = mkPathSegment (495, 6) (498, 6)

      expandPathSegment seg `shouldBe` [(495, 6), (496, 6), (497, 6), (498, 6)]

    it "should expand a reverse horizontal PathSegment" $ do
      let seg = mkPathSegment (498, 6) (495, 6)

      expandPathSegment seg `shouldBe` [(498, 6), (497, 6), (496, 6), (495, 6)]

    it "should expand a vertical PathSegment" $ do
      let seg = mkPathSegment (498, 4) (498, 7)

      expandPathSegment seg `shouldBe` [(498, 4), (498, 5), (498, 6), (498, 7)]

    it "should expand a reverse vertical PathSegment" $ do
      let seg = mkPathSegment (498, 7) (498, 4)

      expandPathSegment seg `shouldBe` [(498, 7), (498, 6), (498, 5), (498, 4)]
