module UtilSpec where

import qualified Data.Set as S

import Test.Hspec

import Util (buildCombinations)

utilSpec :: Spec
utilSpec = do

  describe "buildCombinations" $ do
    it "should compute combinations" $ do
      let xs = [1, 2]
          expected = S.fromList [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]

      S.fromList (buildCombinations 3 0 xs []) `shouldBe` expected
