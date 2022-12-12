{-# LANGUAGE QuasiQuotes #-}

module Day9Spec (day9Spec) where

import Test.Hspec

import Data.List (transpose)
import Text.RawString.QQ

import Day9
import Util (regularParse)

example1 :: String
example1 = [r|R 4
U 4
L 3
|]

day9Spec :: Spec
day9Spec = do
  describe "moveSpecsParser" $ do
    it "should parse MoveSpecs" $ do
      regularParse moveSpecsParser example1 `shouldBe` Right [MoveRightSpec 4, MoveUpSpec 4, MoveLeftSpec 3]

  describe "isAdjacent" $ do
      it "should compute isAdjacent" $ do
        isAdjacent (0, 0) (0, 0) `shouldBe` True
        isAdjacent (0, 1) (0, 0) `shouldBe` True
        isAdjacent (1, 0) (0, 0) `shouldBe` True
        isAdjacent (1, 1) (0, 0) `shouldBe` True
        isAdjacent (2, 1) (0, 0) `shouldBe` False

  describe "genMoves" $ do
    it "should compute Moves from MoveSpecs" $ do
      genMoves [MoveUpSpec 2, MoveRightSpec 3] `shouldBe`
        [MoveUp, MoveUp, MoveRight, MoveRight, MoveRight]

  describe "mkMoveTail" $ do
    it "should compute tail moves" $ do
      getTailMove (0, 0) (0, 0) `shouldBe` Stay
      getTailMove (2, 0) (0, 0) `shouldBe` MoveRight
      getTailMove (0, 2) (0, 0) `shouldBe` MoveUp
      getTailMove (-2, 0) (0, 0) `shouldBe` MoveLeft
      getTailMove (0, -2) (0, 0) `shouldBe` MoveDown
      getTailMove (2, 1) (0, 0) `shouldBe` MoveUpRight
      getTailMove (1, 2) (0, 0) `shouldBe` MoveUpRight
      getTailMove (-1, 2) (0, 0) `shouldBe` MoveUpLeft
      getTailMove (-2, 1) (0, 0) `shouldBe` MoveUpLeft
      getTailMove (-2, -1) (0, 0) `shouldBe` MoveDownLeft
      getTailMove (-1, -2) (0, 0) `shouldBe` MoveDownLeft
      getTailMove (1, -2) (0, 0) `shouldBe` MoveDownRight
      getTailMove (2, -1) (0, 0) `shouldBe` MoveDownRight

  describe "applyMove" $ do
    it "should compute correct position" $ do
      applyMove (0, 0) MoveDownRight `shouldBe` (1, -1)

  describe "applyMoveToHeadAndTail" $ do
    it "should produce the same result as updateKnotPositions" $ do
      let moves = [MoveUp, MoveUp, MoveRight, MoveRight, MoveRight]
          headPos = mkPos 0 0
          tailPos = mkPos 0 0

      let (headPositions, tailPositions) = unzip $ scanl (\(hp, tp) x -> applyMoveToHeadAndTail hp tp x) (headPos, tailPos) moves

      let updatedPositions = transpose $ scanl (flip updateKnotPositions) [headPos, tailPos] moves
      let headPositions' = updatedPositions !! 0
      let tailPositions' = updatedPositions !! 1

      headPositions `shouldBe` headPositions'
      tailPositions `shouldBe` tailPositions'
