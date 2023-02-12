{-# LANGUAGE ScopedTypeVariables #-}

module Day20Spec (day20Spec) where

import qualified Data.Vector as V

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Day20
import Util (regularParse)

compareNumbers :: [IdInt] -> [IdInt] -> Expectation
compareNumbers actual expected = do
  let na = length actual
      ne = length expected
      cycleResetTake n as = take n . dropWhile (\(IdInt _ i) -> i /= 0) . cycle $ as
      current' = cycleResetTake na actual
      expected' = cycleResetTake na expected

  na `shouldBe` ne
  current' `shouldBe` expected'

mixOneTest :: [IdInt] -> IdInt -> IO [IdInt]
mixOneTest idInts idInt = do
  vec <- V.thaw $ V.fromList idInts
  mixOne'' vec idInt
  vec' <- V.freeze vec
  return $ V.toList vec'

day20Spec :: Spec
day20Spec = do

  describe "parseInts" $ do
    it "should parse numbers" $ do
      numbers <- case regularParse parseInts testInput of
        Left e -> fail $ show e
        Right xs -> pure xs

      numbers `shouldBe` [1, 2, -3, 3, -2, 0, 4]

  describe "applyOffset" $ do
    it "should compute a new index by applying an offset to an index in a cyclic list" $ do
      applyOffset 5 2 13 `shouldBe` 0
      applyOffset 5 2 3 `shouldBe` 0

      applyOffset 7 0 1 `shouldBe` 1
      applyOffset 7 0 2 `shouldBe` 2
      applyOffset 7 4 (-2) `shouldBe` 2
      applyOffset 7 1 (-3) `shouldBe` 5

      applyOffset 8 2 7 `shouldBe` 1
      applyOffset 8 2 (-3) `shouldBe` 7
      applyOffset 8 2 (-13) `shouldBe` 5
      applyOffset 6 3 (-2) `shouldBe` 1
      applyOffset 7 6 4 `shouldBe` 3
      applyOffset 7 3 2 `shouldBe` 5
      applyOffset 7 3 (-2) `shouldBe` 1

  describe "mixOne" $ do
    it "should move an element by a specific offset" $ do
      let xs = [IdInt 0 2, IdInt 1 7, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          expected = [IdInt 0 2, IdInt 2 1, IdInt 3 3, IdInt 1 7, IdInt 4 5]

      let actual = mixOne xs (IdInt 1 7)

      actual `shouldBe` expected

    it "should move elements by a specific offset" $ do
      let xs1 = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          xs2 = mixOne xs1 (IdInt 0 1)
          xs3 = mixOne xs2 (IdInt 1 2)
          xs4 = mixOne xs3 (IdInt 2 (-3))
          xs5 = mixOne xs4 (IdInt 3 3)
          xs6 = mixOne xs5 (IdInt 4 (-2))
          xs7 = mixOne xs6 (IdInt 5 0)
          xs8 = mixOne xs7 (IdInt 6 4)

      xs2 `shouldBe` [IdInt 1 2, IdInt 0 1, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
      xs3 `shouldBe` [IdInt 0 1, IdInt 2 (-3), IdInt 1 2, IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
      xs4 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 3 3, IdInt 4 (-2), IdInt 2 (-3), IdInt 5 0, IdInt 6 4]
      xs5 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 4 (-2), IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4]
      xs6 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4, IdInt 4 (-2)]
      xs7 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4, IdInt 4 (-2)]
      xs8 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

  describe "mix" $ do
    it "should mix a list of IdInt" $ do
      let idInts = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          expected = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

      mix idInts idInts `shouldBe` expected

  describe "shift" $ do
    prop "should return same list if offset is zero" $ \(xs :: [Int], i) -> do
      shift 0 i xs `shouldBe` xs

    it "should shift an element forward by a specified amount" $ do
      let xs = [2, 1, 3, 4, -1]

      shift 1 0 xs `shouldBe` [1, 2, 3, 4, -1]
      shift 2 0 xs `shouldBe` [1, 3, 2, 4, -1]
      shift 1 4 xs `shouldBe` [-1, 1, 3, 4, 2]
      shift 3 3 xs `shouldBe` [1, 4, 3, -1, 2]
      shift 13 3 xs `shouldBe` [-1, 4, 2, 1, 3]

    it "should shift an element backward by a specified amount" $ do
      let xs = [2, 1, 3, 4, -1]

      shift (-1) 4 xs `shouldBe` [2, 1, 3, -1, 4]
      shift (-2) 4 xs `shouldBe` [2, 1, -1, 3, 4]
      shift (-1) 0 xs `shouldBe` [-1, 1, 3, 4, 2]
      shift (-2) 0 xs `shouldBe` [-1, 1, 3, 2, 4]
      shift (-3) 1 xs `shouldBe` [-1, 2, 3, 1, 4]
      shift (-13) 1 xs `shouldBe` [3, 4, -1, 1, 2]

  describe "mix'" $ do
    it "should mix a list of IdInt" $ do
      let idInts = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          expected = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

      idInts' <- mix' idInts

      compareNumbers idInts' expected

  describe "mix''" $ do
    it "should mix a list of IdInt" $ do
      let idInts = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          expected = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

      idInts' <- mix'' idInts idInts

      compareNumbers idInts' expected

  describe "mixOne func" $ do
    it "should mix one (1)" $ do
      let idInts = [IdInt 0 0, IdInt 1 4, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          idInt = IdInt 1 4
          expected = [IdInt 1 4, IdInt 2 1, IdInt 3 3, IdInt 4 5, IdInt 0 0]

      idInts' <- mixOneTest idInts idInt

      compareNumbers idInts' expected

    it "should mix one (2)" $ do
      let idInts = [IdInt 0 0, IdInt 1 3, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          expected = [IdInt 0 0, IdInt 2 1, IdInt 3 3, IdInt 4 5, IdInt 1 3]
          idInt = idInts !! 1

      idInts' <- mixOneTest idInts idInt

      compareNumbers idInts' expected

    it "should mix one (3)" $ do
      let idInts = [IdInt 0 0, IdInt 1 15, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          expected = [IdInt 0 0, IdInt 2 1, IdInt 3 3, IdInt 4 5, IdInt 1 15]
          idInt = idInts !! 1

      idInts' <- mixOneTest idInts idInt

      compareNumbers idInts' expected
