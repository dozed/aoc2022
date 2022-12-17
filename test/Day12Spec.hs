module Day12Spec (day12Spec) where

import Test.Hspec

import Day12

day12Spec :: Spec
day12Spec = do
  describe "getStartPos" $ do
    let field = mkField testInput1

    it "should get start position" $ do
      getStartPos field `shouldBe` (0, 0)

    it "should get end position" $ do
      getEndPos field `shouldBe` (2, 5)

