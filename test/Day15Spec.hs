{-# LANGUAGE QuasiQuotes #-}

module Day15Spec (day15Spec) where

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

  describe "mkSensorInfo" $
    it "should create SensorInfo with manhattan distance" $
      mkSensorInfo (3, 2) (7, 1) `shouldBe` Info (3, 2) (7, 1) 5

  describe "lineParser" $ do
    it "should parse an Info" $ do
      let txt = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
      regularParse infoParser txt `shouldBe` Right (Info (2, 18) (-2, 15) 7)

  describe "linesParser" $ do
    it "should parse a list of Info" $ do
      regularParse infosParser testInput1 `shouldBe` Right [Info (2, 18) (-2, 15) 7, Info (9, 16) (10, 16) 1]

  describe "getManhattanDistance" $ do
    it "should compute the manhattan distance for two Pos values" $ do
      let p1 = (8, 7)
          p2 = (2, 10)

      getManhattanDistance p1 p2 `shouldBe` 9
      getManhattanDistance p2 p1 `shouldBe` 9

    prop "should be symmetric" $ \(p1, p2) ->
      getManhattanDistance p1 p2 `shouldBe` getManhattanDistance p2 p1

  describe "getMinXByInfo" $ do
    it "should compute minimum covered x distance" $ do
      getMinXByInfo (Info (8, 7) (2, 10) 9) `shouldBe` (-1)

  describe "getMaxXByInfo" $ do
    it "should compute maximum covered x distance" $ do
      getMaxXByInfo (Info (8, 7) (2, 10) 9) `shouldBe` 17

  describe "getMinYByInfo" $ do
    it "should compute minimum covered y distance" $ do
      getMinYByInfo (Info (8, 7) (2, 10) 9) `shouldBe` (-2)

  describe "getMaxYByInfo" $ do
    it "should compute maximum covered y distance" $ do
      getMaxYByInfo (Info (8, 7) (2, 10) 9) `shouldBe` 16

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

  describe "countNonBeaconPositionsInRow" $ do
    it "should count non-beacon position in row" $ do
      infos <- case regularParse infosParser testInput of
        Left e -> fail $ show e
        Right xs -> pure xs
    
      let beaconPositions = S.fromList $ map (\(Info _ bp _) -> bp) infos
          coveredPositions = S.unions $ map (\(Info sp bp _) -> getCoveredPositions sp (getManhattanDistance sp bp)) infos

      countNonBeaconPositionsInRow 10 beaconPositions coveredPositions `shouldBe` 26

  describe "countNonBeaconPositionsInRow'" $ do
    it "should count non-beacon position in row" $ do
      infos <- case regularParse infosParser testInput of
        Left e -> fail $ show e
        Right xs -> pure xs

      let beaconPositions = S.fromList $ map (\(Info _ bp _) -> bp) infos
          -- coveredPositions = S.unions $ map (\(Info sp bp) -> getCoveredPositions sp (getManhattanDistance sp bp)) infos

      countNonBeaconPositionsInRow' 10 infos beaconPositions `shouldBe` 26

  describe "isCoveredBySensor" $ do
    it "should detect covered position" $ do
      isCoveredBySensor (Info (8, 7) (2, 10) 9) (2, 10) `shouldBe` True

    it "should detect non-covered position" $ do
      isCoveredBySensor (Info (8, 7) (2, 10) 9) (2, 11) `shouldBe` False

  describe "isCoveredBySensor'" $ do
    it "should detect covered position" $ do
      isCoveredBySensor' (Info (8, 7) (2, 10) 9) (2, 10) `shouldBe` True

    it "should detect non-covered position" $ do
      isCoveredBySensor' (Info (8, 7) (2, 10) 9) (2, 11) `shouldBe` False

  describe "getSkippableX" $ do
    it "should skip all covered x positions" $ do
      let pos = (1, 4)
          sensor = mkSensorInfo (3, 2) (7, 1)

      getSkippableX sensor pos `shouldBe` Just 6

      let pos' = (5, 4)
          sensor' = mkSensorInfo (3, 2) (7, 1)

      getSkippableX sensor' pos' `shouldBe` Just 2

    it "should not skip for a non-covered position" $ do
      let pos = (-2, 4)
          sensor = mkSensorInfo (3, 2) (7, 1)

      getSkippableX sensor pos `shouldBe` Nothing

      let pos' = (7, 0)
          sensor' = mkSensorInfo (3, 2) (7, 1)

      getSkippableX sensor' pos' `shouldBe` Nothing

  describe "getMaximumSkippableX" $ do
    it "should find maximum skippable x's for multiple Infos" $ do
      let sensors = [mkSensorInfo (0, 6) (0, 4), mkSensorInfo (3, 2) (7, 1), mkSensorInfo (12, 1) (13, 0)]

      getMaximumSkippableX sensors (1, 4) `shouldBe` Just 6
      getMaximumSkippableX sensors (5, 4) `shouldBe` Just 2
      getMaximumSkippableX sensors (7, 4) `shouldBe` Nothing
      getMaximumSkippableX sensors (11, 0) `shouldBe` Just 3
      getMaximumSkippableX sensors (2, 6) `shouldBe` Just 3

  describe "findUncoveredPos" $ do
    it "should find uncovered position" $ do
      let input = testInput

      infos <- case regularParse infosParser input of
        Left e -> fail $ show e
        Right xs -> pure xs

      findUncoveredPos 20 infos (0, 0) `shouldBe` Just (14, 11)

  describe "getTuningSignal" $ do
    it "should compute the tuning signal for a position" $ do
      getTuningSignal (14, 11) `shouldBe` 56000011
      getTuningSignal (2595657, 2753392) `shouldBe` 10382630753392
