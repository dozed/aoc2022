module Day18Spec (day18Spec) where

import Data.Set (Set)
import qualified Data.Set as S

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
      droplet1 <- case regularParse positionsParser miniInput of
        Left msg -> fail $ show msg
        Right xs -> pure $ S.fromList xs

      countFreeSides droplet1 `shouldBe` 10

      droplet2 <- case regularParse positionsParser testInput of
        Left msg -> fail $ show msg
        Right xs -> pure $ S.fromList xs

      countFreeSides droplet2 `shouldBe` 64

  describe "getBoundingArea" $ do
    it "should get the BoundingArea for a droplet" $ do
      droplet <- case regularParse positionsParser testInput of
        Left msg -> fail $ show msg
        Right xs -> pure $ S.fromList xs

      getBoundingArea droplet `shouldBe` (0, 4, 0, 4, 0, 7)

  describe "getStartPos" $ do
    it "should get the starting position" $ do
      droplet <- case regularParse positionsParser testInput of
        Left msg -> fail $ show msg
        Right xs -> pure $ S.fromList xs

      let boundingArea = getBoundingArea droplet

      getStartPos boundingArea `shouldBe` (0, 0, 0)

  describe "getReachables" $ do
    it "should get all reachables for a given BoundingArea and Droplet" $ do
      let droplet = S.fromList [(1, 1, 1)]
          boundingArea = getBoundingArea droplet
          outerPositions = getReachables boundingArea droplet

      outerPositions `shouldBe` S.fromList [(0,0,0),(0,0,1),(0,0,2),(0,1,0),(0,1,1),(0,1,2),(0,2,0),(0,2,1),(0,2,2),(1,0,0),(1,0,1),(1,0,2),(1,1,0),(1,1,2),(1,2,0),(1,2,1),(1,2,2),(2,0,0),(2,0,1),(2,0,2),(2,1,0),(2,1,1),(2,1,2),(2,2,0),(2,2,1),(2,2,2)]
      S.intersection outerPositions droplet `shouldBe` S.empty

  describe "countAdjacentPositions" $ do
    it "should compute adjacent positions" $ do
      let dropletPos = (1, 1, 1)
          droplet = S.fromList [dropletPos]
          boundingArea = getBoundingArea droplet
          outerPositions = getReachables boundingArea droplet

      countAdjacentPositions outerPositions dropletPos `shouldBe` 6

  describe "countOuterFreeSides" $ do
    it "should compute the free sides on the outside" $ do
      droplet <- case regularParse positionsParser testInput of
        Left msg -> fail $ show msg
        Right xs -> pure $ S.fromList xs

      countOuterFreeSides droplet `shouldBe` 58
