module Day14Spec (day14Spec) where

import Control.Exception (evaluate)
import qualified Data.Set as S

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

      expandPathSegment seg `shouldBe` S.fromList [(495, 6), (496, 6), (497, 6), (498, 6)]

    it "should expand a reverse horizontal PathSegment" $ do
      let seg = mkPathSegment (498, 6) (495, 6)

      expandPathSegment seg `shouldBe` S.fromList [(498, 6), (497, 6), (496, 6), (495, 6)]

    it "should expand a vertical PathSegment" $ do
      let seg = mkPathSegment (498, 4) (498, 7)

      expandPathSegment seg `shouldBe` S.fromList [(498, 4), (498, 5), (498, 6), (498, 7)]

    it "should expand a reverse vertical PathSegment" $ do
      let seg = mkPathSegment (498, 7) (498, 4)

      expandPathSegment seg `shouldBe` S.fromList [(498, 7), (498, 6), (498, 5), (498, 4)]

  describe "expandPath" $ do
    it "should expand a Path" $ do
      let path = [(498, 4), (498, 6), (496, 6)]
      let expected = S.fromList [(498, 4), (498, 5), (498, 6), (498, 6), (497, 6), (496, 6)]

      expandPath path `shouldBe` expected

    it "should expand another Path" $ do
      let path = [(503, 4), (502, 4), (502, 9), (500, 9)]
      let expected = S.fromList [(503, 4), (502, 4), (502, 5), (502, 6), (502, 7), (502, 8), (502, 9), (501, 9), (500, 9)]

      expandPath path `shouldBe` expected

  describe "expandPaths" $ do
    it "should expand a list of Paths" $ do
      let paths = [
              [(498, 4), (498, 6), (496, 6)],
              [(503, 4), (502, 4), (502, 9), (500, 9)]
            ]

      let expected = S.union
            (S.fromList [(498, 4), (498, 5), (498, 6), (498, 6), (497, 6), (496, 6)])
            (S.fromList [(503, 4), (502, 4), (502, 5), (502, 6), (502, 7), (502, 8), (502, 9), (501, 9), (500, 9)])

      expandPaths paths `shouldBe` expected

  describe "getDownPos" $ it "should get position below" $ getDownPos (1, 0) `shouldBe` (1, 1)
  describe "getDownLeftPos" $ it "should get position below left" $ getDownLeftPos (1, 0) `shouldBe` (0, 1)
  describe "getDownRightPos" $ it "should get position below right" $ getDownRightPos (1, 0) `shouldBe` (2, 1)

  describe "isBlockedPos" $ do
    it "should detect a blocked position" $ isBlockedPos (S.fromList [(0, 0)]) (0, 0) `shouldBe` True
    it "should detect a non-blocked position" $ isBlockedPos (S.fromList [(0, 0)]) (1, 1) `shouldBe` False

  describe "isComeToRest" $ do
    it "should detect a blocked pos" $ do
      let field = S.fromList [(0, 1), (1, 1), (2, 1)]
      isComeToRest field (1, 0) `shouldBe` True

    it "should detect a non-blocked pos" $ do
      let field1 = S.fromList [(0, 1), (1, 1)]
      isComeToRest field1 (1, 0) `shouldBe` False

      let field2 = S.fromList [(0, 1), (2, 1)]
      isComeToRest field2 (1, 0) `shouldBe` False

      let field3 = S.fromList [(1, 1), (2, 1)]
      isComeToRest field3 (1, 0) `shouldBe` False

