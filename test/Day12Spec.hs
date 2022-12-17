module Day12Spec (day12Spec) where

import qualified Data.Set as S

import Test.Hspec

import Day12

day12Spec :: Spec
day12Spec = do
  let field = mkField testInput1

  describe "getHeight" $ do
    it "should compute height for 'S'" $ getHeight 'S' `shouldBe` 'a'
    it "should compute height for 'E'" $ getHeight 'E' `shouldBe` 'z'
    it "should compute height for 'c'" $ getHeight 'c' `shouldBe` 'c'

  describe "incrCell" $ do
    it "should get next higher cell value" $ do
      incrCell 'a' `shouldBe` 'b'

  describe "getPos" $ do
    it "should return value for cell value which is on the field" $ do
      getPos 'a' field `shouldBe` Just (0, 1)

    it "should return Nothing for cell value which is not on the field" $ do
      getPos 'X' field `shouldBe` Nothing

  describe "getStartPos" $ do
    it "should get start position" $ do
      getStartPos field `shouldBe` Just (0, 0)

    it "should get end position" $ do
      getEndPos field `shouldBe` Just (2, 5)

  describe "getCell" $ do
    it "should get cell" $ do
      getCell field (0, 0) `shouldBe` Just 'S'
      getCell field (2, 5) `shouldBe` Just 'E'
      getCell field (1, 0) `shouldBe` Just 'a'
      getCell field (0, 1) `shouldBe` Just 'a'

    it "should return Nothing for out-of-field cell" $ do
      getCell field (12, 15) `shouldBe` Nothing

  describe "getAdjacentPositions" $ do
    it "should compute adjacent positions" $ do
      getAdjacentPositions (0, 0) `shouldBe` S.fromList [(-1, 0), (1, 0), (0, -1), (0, 1)]

  describe "getReachablePositions" $ do
    it "should compute reachable positions" $ do
      getReachablePositions field (0, 0) `shouldBe` S.fromList [(0, 1), (1, 0)]
