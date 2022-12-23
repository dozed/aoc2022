{-# LANGUAGE QuasiQuotes #-}

module Day15Spec (day15Spec) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as S
import Text.RawString.QQ

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

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

  describe "getManhattanDistance" $ do
    it "should compute the manhattan distance for two Pos values" $ do
      let p1 = (8, 7)
          p2 = (2, 10)

      getManhattanDistance p1 p2 `shouldBe` 9
      getManhattanDistance p2 p1 `shouldBe` 9

    prop "should be symmetric" $ \(p1, p2) ->
      getManhattanDistance p1 p2 `shouldBe` getManhattanDistance p2 p1

  describe "getCoveredRowPoints" $ do
    it "should compute covered points in the current row in a specific distance" $ do
      let p = (8, 0)
          n = 2

      getCoveredRowPoints p n `shouldBe` S.fromList [(6, 0), (7, 0), (8, 0), (9, 0), (10, 0)]

  describe "getUpwardPos" $ do
    it "should compute an upward point for a given distance" $ do
      getUpwardPos (8, 7) `shouldBe` (8, 6)

  describe "getDownwardPos" $ do
    it "should compute a downward point for a given distance" $ do
      getDownwardPos (8, 7) `shouldBe` (8, 8)

  describe "getCoveredPositions" $ do
    it "should compute covered positions" $ do

      getCoveredPositions (8, 7) 2 `shouldBe` S.fromList [
                          (8, 5),
                  (7, 6), (8, 6), (9, 6),
          (6, 7), (7, 7), (8, 7), (9, 7), (10, 7),
                  (7, 8), (8, 8), (9, 8),
                          (8, 9)
        ]

  describe "getNonBeaconPositionsInRow" $ do
    it "should compute non beacon position in row" $ do
      infos <- case regularParse infosParser testInput of
        Left e -> fail $ show e
        Right xs -> pure xs
    
      let beaconPositions = S.fromList $ map (\(Info _ bp) -> bp) infos
          coveredPositions = S.unions $ map (\(Info sp bp) -> getCoveredPositions sp (getManhattanDistance sp bp)) infos

      getNonBeaconPositionsInRow 10 beaconPositions coveredPositions `shouldBe` 26
