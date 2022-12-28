module UtilSpec where

import qualified Data.Set as S

import Test.Hspec

import Util (buildCombinations, buildPermutations, findElem)

utilSpec :: Spec
utilSpec = do

  describe "buildCombinations" $ do
    it "should compute combinations" $ do
      let xs = [1, 2]
          expected = S.fromList [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]

      S.fromList (buildCombinations 3 0 xs []) `shouldBe` expected

  describe "buildPermutations" $ do
    it "should compute permutations" $ do
      let xs = [1, 2, 3]
          expected = [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

      buildPermutations xs `shouldBe` expected

  describe "findElem" $ do
    it "should extract an element from a list" $ do
      findElem (== 2) [1, 2, 3] `shouldBe` Just (2, [1, 3])

    it "should not extract an element when not in a list" $ do
      findElem (== 5) [1, 2, 3] `shouldBe` Nothing
