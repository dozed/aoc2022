module UtilSpec where

import qualified Data.Set as S

import Test.Hspec

import Util (buildCombinations, buildPermutations)

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
