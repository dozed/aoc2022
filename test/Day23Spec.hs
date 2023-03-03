module Day23Spec where

import Day23

import Test.Hspec

day23Spec :: Spec
day23Spec = do

  describe "getAdjacentPos" $ do
    it "should get adjacent position" $ do
      getAdjacentPos N (2, 2) `shouldBe` (2, 1)
      getAdjacentPos S (2, 2) `shouldBe` (2, 3)
      getAdjacentPos E (2, 2) `shouldBe` (3, 2)
      getAdjacentPos W (2, 2) `shouldBe` (1, 2)
      getAdjacentPos NE (2, 2) `shouldBe` (3, 1)
      getAdjacentPos NW (2, 2) `shouldBe` (1, 1)
      getAdjacentPos SE (2, 2) `shouldBe` (3, 3)
      getAdjacentPos SW (2, 2) `shouldBe` (1, 3)

  describe "shiftDirections" $ do
    it "should shift directions" $ do
      shiftDirections [N, S, W, E] `shouldBe` [S, W, E, N]
