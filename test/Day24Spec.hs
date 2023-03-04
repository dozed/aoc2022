{-# LANGUAGE OverloadedRecordDot #-}

module Day24Spec where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.Hspec

import Day24

testInputState1 :: [String]
testInputState1 = [
    "#.#####",
    "#.....#",
    "#.>...#",
    "#.....#",
    "#.....#",
    "#...v.#",
    "#####.#"
  ]

testInputState2 :: [String]
testInputState2 = [
    "#.#####",
    "#...v.#",
    "#..>..#",
    "#.....#",
    "#.....#",
    "#.....#",
    "#####.#"
  ]

testInputState4 :: [String]
testInputState4 = [
    "#.#####",
    "#.....#",
    "#....>#",
    "#...v.#",
    "#.....#",
    "#.....#",
    "#####.#"
  ]

testInputState5 :: [String]
testInputState5 = [
    "#.#####",
    "#.....#",
    "#>....#",
    "#.....#",
    "#...v.#",
    "#.....#",
    "#####.#"
  ]

day24Spec :: Spec
day24Spec = do

  describe "readField" $ do
    it "should read a field" $ do
      let field = readField testInput

      field.startPos `shouldBe` (2, 1)
      field.endPos `shouldBe` (6, 7)
      field.blizzards `shouldBe` M.fromList [((2, 3), [E]), ((5, 5), [S])]
      S.size field.walls `shouldBe` 22

  describe "getAdjacentPos" $ do
    it "should get an adjacent position" $ do
      getAdjacentPos (2, 2) N `shouldBe` (2, 1)
      getAdjacentPos (2, 2) S `shouldBe` (2, 3)
      getAdjacentPos (2, 2) E `shouldBe` (3, 2)
      getAdjacentPos (2, 2) W `shouldBe` (1, 2)

  describe "isBlizzardAt" $ do
    it "should detect a blizzard" $ do
      let field = readField testInput

      isBlizzardAt field (2, 3) `shouldBe` True
      isBlizzardAt field (3, 4) `shouldBe` False

  describe "isWallAt" $ do
    it "should detect a wall" $ do
      let field = readField testInput

      isWallAt field (1, 3) `shouldBe` True
      isWallAt field (3, 4) `shouldBe` False

  describe "getBlizzardPositions" $ do
    it "should get all blizzard positions" $ do
      let field = readField testInput

      getBlizzardPositions field `shouldBe` [(2, 3), (5, 5)]

  describe "wrapPos" $ do
    it "should wrap a position around" $ do
      let field = readField testInput

      wrapPos field (6, 3) E `shouldBe` (2, 3)
      wrapPos field (5, 6) S `shouldBe` (5, 2)

  describe "moveBlizzardsAtPos" $ do
    it "should move blizzards at position" $ do
      let field = readField testInput

      isBlizzardAt field (3, 3) `shouldBe` False
      isBlizzardAt field (2, 3) `shouldBe` True

      let field' = moveBlizzardsAtPos field (2, 3)

      isBlizzardAt field' (3, 3) `shouldBe` True
      isBlizzardAt field' (2, 3) `shouldBe` False

    it "should wrap a blizzard around" $ do
      let field = readField testInput

      isBlizzardAt field (2, 3) `shouldBe` True

      let field1 = moveBlizzardsAtPos field (2, 3)
      isBlizzardAt field1 (2, 3) `shouldBe` False
      isBlizzardAt field1 (3, 3) `shouldBe` True

      let field2 = moveBlizzardsAtPos field1 (3, 3)
      isBlizzardAt field2 (3, 3) `shouldBe` False
      isBlizzardAt field2 (4, 3) `shouldBe` True

      let field3 = moveBlizzardsAtPos field2 (4, 3)
      isBlizzardAt field3 (4, 3) `shouldBe` False
      isBlizzardAt field3 (5, 3) `shouldBe` True

      let field4 = moveBlizzardsAtPos field3 (5, 3)
      isBlizzardAt field4 (5, 3) `shouldBe` False
      isBlizzardAt field4 (6, 3) `shouldBe` True

      let field5 = moveBlizzardsAtPos field4 (6, 3)
      isBlizzardAt field5 (6, 3) `shouldBe` False
      isBlizzardAt field5 (2, 3) `shouldBe` True

  describe "moveBlizzards" $ do
    it "should move all blizzards (1 step)" $ do
      let field = readField testInput
          expected = readField testInputState1

      let field' = moveBlizzards field

      field' `shouldBe` expected

    it "should move all blizzards (2 steps)" $ do
      let field = readField testInput
          expected = readField testInputState2

      let field' = iterate moveBlizzards field !! 2

      field' `shouldBe` expected

    it "should move all blizzards (3 steps)" $ do
      let field = readField testInput

      let field' = iterate moveBlizzards field !! 3

      field'.blizzards `shouldBe` M.fromList [((5, 3), [S, E])]

    it "should move all blizzards (4 steps)" $ do
      let field = readField testInput
          expected = readField testInputState4

      let field' = iterate moveBlizzards field !! 4

      field' `shouldBe` expected

    it "should move all blizzards (5 steps)" $ do
      let field = readField testInput
          expected = readField testInputState5

      let field' = iterate moveBlizzards field !! 5

      field' `shouldBe` expected
