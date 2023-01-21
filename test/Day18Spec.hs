module Day18Spec (day18Spec) where

import Test.Hspec

import Day18
import Util (regularParse)

day18Spec :: Spec
day18Spec = do

  describe "dropletParser" $ do
    it "should parse a Droplet" $ do
      regularParse positionsParser miniInput `shouldBe` Right [(1, 1, 1), (2, 1, 1)]

  describe "isAdjacent" $ do
    it "should detect adjacent positions" $ do
      isAdjacent (1, 1, 1) (2, 1, 1) `shouldBe` True

    it "should detect non-adjacent positions" $ do
      isAdjacent (1, 1, 1) (3, 1, 1) `shouldBe` False
      isAdjacent (1, 2, 2) (2, 1, 2) `shouldBe` False

  describe "countFreeSides'" $ do
    it "should count the free sides of a cube given a list of other cubes" $ do
      countFreeSides' (2, 1, 1) [(1, 1, 1)] `shouldBe` 5

  describe "countFreeSides" $ do
    it "should count the free sides of a list of cubes" $ do
      cubes1 <- case regularParse positionsParser miniInput of
        Left msg -> fail $ show msg
        Right xs -> pure xs

      countFreeSides cubes1 `shouldBe` 10

      cubes2 <- case regularParse positionsParser testInput of
        Left msg -> fail $ show msg
        Right xs -> pure xs

      countFreeSides cubes2 `shouldBe` 64

  describe "getBoundingArea" $ do
    it "should get the BoundingArea for a droplet" $ do
      cubes <- case regularParse positionsParser testInput of
        Left msg -> fail $ show msg
        Right xs -> pure xs

      getBoundingArea cubes `shouldBe` (0, 4, 0, 4, 0, 7)
