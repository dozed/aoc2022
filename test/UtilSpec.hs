module UtilSpec (utilSpec) where

import qualified Data.Set as S

import Test.Hspec

import Util (buildCombinations, buildPermutations, findElem, insertAtIndex, move, removeAtIndex, swap)

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

  describe "removeAtIndex" $ do
    it "should remove an element at a specific index from a list" $ do
      let xs = [0, 1, 2, 3, 4, 5]

      removeAtIndex 0 xs `shouldBe` [1, 2, 3, 4, 5]
      removeAtIndex 2 xs `shouldBe` [0, 1, 3, 4, 5]
      removeAtIndex 5 xs `shouldBe` [0, 1, 2, 3, 4]

  describe "insertAtIndex" $ do
    it "should remove an element at a specific index from a list" $ do
      let xs = [0, 1, 2, 3, 4, 5]

      insertAtIndex 0 10 xs `shouldBe` [10, 0, 1, 2, 3, 4, 5]
      insertAtIndex 2 10 xs `shouldBe` [0, 1, 10, 2, 3, 4, 5]
      insertAtIndex 5 10 xs `shouldBe` [0, 1, 2, 3, 4, 10, 5]
      insertAtIndex 6 10 xs `shouldBe` [0, 1, 2, 3, 4, 5, 10]

  describe "move" $ do
    it "should move an element from an index in a list to another index" $ do
      let xs = [0, 1, 2, 3, 4, 5, 6]

      move 3 0 xs `shouldBe` [3, 0, 1, 2, 4, 5, 6]
      move 3 1 xs `shouldBe` [0, 3, 1, 2, 4, 5, 6]
      move 3 2 xs `shouldBe` [0, 1, 3, 2, 4, 5, 6]
      move 3 3 xs `shouldBe` [0, 1, 2, 3, 4, 5, 6]
      move 3 4 xs `shouldBe` [0, 1, 2, 4, 3, 5, 6]
      move 3 5 xs `shouldBe` [0, 1, 2, 4, 5, 3, 6]
      move 3 6 xs `shouldBe` [0, 1, 2, 4, 5, 6, 3]

  describe "swap" $ do
    it "should swap two positions" $ do
      let xs = [0, 1, 2, 3, 4, 5]

      swap 2 4 xs `shouldBe` [0, 1, 4, 3, 2, 5]
