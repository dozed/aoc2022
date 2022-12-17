module Day12Spec (day12Spec) where

import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Set as S

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (property, (==>))

import Day12

day12Spec :: Spec
day12Spec = do
  let field = mkField testInput1

  describe "getHeight" $ do
    it "should compute height for 'S'" $ getHeight 'S' `shouldBe` 'a'
    it "should compute height for 'E'" $ getHeight 'E' `shouldBe` 'z'
    it "should compute height for 'c'" $ getHeight 'c' `shouldBe` 'c'

  describe "isEnd" $ do
    prop "should not detect non-E as end cell value" $ \c -> (c /= 'E') ==> isEnd c `shouldBe` False
    it "should detect E as end cell value" $ isEnd 'E' `shouldBe` True

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

  describe "searchPaths" $ do
    it "should find the shortest path from S to E" $ do
      startPos <- case getStartPos field of
        (Just pos) -> pure pos
        Nothing -> fail "could not get start position"

      let paths = searchPaths field startPos [] S.empty
          shortestPath = minimumBy (compare `on` length) paths
          shortestPathLength = length shortestPath
          shortestPathSteps = shortestPathLength - 1

      shortestPath `shouldBe` [(0,0),(0,1),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(3,7),(2,7),(1,7),(0,7),(0,6),(0,5),(0,4),(0,3),(1,3),(2,3),(3,3),(3,4),(3,5),(3,6),(2,6),(1,6),(1,5),(1,4),(2,4),(2,5)]
      shortestPathLength `shouldBe` 32
      shortestPathSteps `shouldBe` 31
