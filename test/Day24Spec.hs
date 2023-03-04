module Day24Spec where

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

  describe "isFloorAt" $ do
    it "should detect a floor" $ do
      let field = readField testInput

      isFloorAt field (2, 1) `shouldBe` True
      isFloorAt field (3, 2) `shouldBe` True
      isFloorAt field (6, 7) `shouldBe` True
      isFloorAt field (1, 3) `shouldBe` False
      isFloorAt field (2, 3) `shouldBe` False

  describe "getBlizzards" $ do
    it "should get all blizzards" $ do
      let field = readField testInput

      getBlizzards field `shouldBe` [((2, 3), E), ((5, 5), S)]

  describe "wrapPos" $ do
    it "should wrap a position around" $ do
      let field = readField testInput

      wrapPos field (6, 3) E `shouldBe` (2, 3)
      wrapPos field (5, 6) S `shouldBe` (5, 2)

  describe "moveBlizzard" $ do
    it "should move a blizzard" $ do
      let field = readField testInput

      isBlizzardAt field (2, 3) `shouldBe` True
      isFloorAt field (3, 3) `shouldBe` True

      let (field', pos') = moveBlizzard field (2, 3) E

      pos' `shouldBe` (3, 3)
      isBlizzardAt field' (3, 3) `shouldBe` True
      isFloorAt field' (2, 3) `shouldBe` True

    it "should wrap a blizzard around" $ do
      let field = readField testInput

      let (field', pos') = moveBlizzard field (6, 3) E

      pos' `shouldBe` (2, 3)
      isBlizzardAt field' (2, 3) `shouldBe` True
      isFloorAt field' (6, 3) `shouldBe` True

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
