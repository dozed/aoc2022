module Day24Spec where

import qualified Data.Map as M

import Test.Hspec

import Day24

day24Spec :: Spec
day24Spec = do

  describe "getAdjacentPos" $ do
    it "should get an adjacent position" $ do
      getAdjacentPos (2, 2) N `shouldBe` (2, 1)
      getAdjacentPos (2, 2) S `shouldBe` (2, 3)
      getAdjacentPos (2, 2) E `shouldBe` (3, 2)
      getAdjacentPos (2, 2) W `shouldBe` (1, 2)

  describe "isBlizzardAt" $ do
    it "should detect a blizzard" $ do
      let input = testInput
          field = readField input

      isBlizzardAt field (2, 3) `shouldBe` True
      isBlizzardAt field (3, 4) `shouldBe` False

  describe "isWallAt" $ do
    it "should detect a wall" $ do
      let input = testInput
          field = readField input

      isWallAt field (1, 3) `shouldBe` True
      isWallAt field (3, 4) `shouldBe` False

  describe "isFloorAt" $ do
    it "should detect a floor" $ do
      let input = testInput
          field = readField input

      isFloorAt field (2, 1) `shouldBe` True
      isFloorAt field (3, 2) `shouldBe` True
      isFloorAt field (6, 7) `shouldBe` True
      isFloorAt field (1, 3) `shouldBe` False
      isFloorAt field (2, 3) `shouldBe` False

  describe "wrapPos" $ do
    it "should wrap a position around" $ do
      let input = testInput
          field = readField input

      wrapPos field (6, 3) E `shouldBe` (2, 3)
      wrapPos field (5, 6) S `shouldBe` (5, 2)

  describe "moveBlizzard" $ do
    it "should move a blizzard" $ do
      let input = testInput
          field = readField input

      isBlizzardAt field (2, 3) `shouldBe` True
      isFloorAt field (3, 3) `shouldBe` True

      let (field', pos') = moveBlizzard field (2, 3) E

      pos' `shouldBe` (3, 3)
      isBlizzardAt field' (3, 3) `shouldBe` True
      isFloorAt field' (2, 3) `shouldBe` True

    it "should wrap a blizzard around" $ do
      let input = testInput
          field = readField input

      let (field', pos') = moveBlizzard field (6, 3) E

      pos' `shouldBe` (2, 3)
      isBlizzardAt field' (2, 3) `shouldBe` True
      isFloorAt field' (6, 3) `shouldBe` True
