{-# LANGUAGE ScopedTypeVariables #-}

module Day20Spec (day20Spec) where

import Control.Monad (forM_)
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
  mixOne vec idInt
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

  describe "getSmallOffset" $ do
    it "should compute the small equivalent offset" $ do
      getSmallOffset 3 5 `shouldBe` 3
      getSmallOffset 7 5 `shouldBe` 3
      getSmallOffset 11 5 `shouldBe` 3
      getSmallOffset 15 5 `shouldBe` 3
      getSmallOffset 19 5 `shouldBe` 3
      getSmallOffset 23 5 `shouldBe` 3
      getSmallOffset 27 5 `shouldBe` 3
      getSmallOffset 31 5 `shouldBe` 3

    it "should compute the small equivalent offset (2)" $ do
      forM_ [3, 7, 11, 15, 19, 23, 27, 31] $ \offset ->
        getSmallOffset offset 5 `shouldBe` 3

  describe "shift" $ do
    prop "should return same list if offset is zero" $ \(xs :: [Int], i) -> do
      vec <- V.thaw . V.fromList $ xs
      shift 0 i vec
      xs' <- V.toList <$> V.freeze vec

      xs' `shouldBe` xs

  describe "mixOne" $ do
    it "should mix one forward (1)" $ do
      let idInts = [IdInt 0 0, IdInt 1 4, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          idInt = IdInt 1 4
          expected = [IdInt 1 4, IdInt 2 1, IdInt 3 3, IdInt 4 5, IdInt 0 0]

      actual <- mixOneTest idInts idInt

      compareNumbers actual expected

    it "should mix one forward (2)" $ do
      let idInts = [IdInt 0 0, IdInt 1 3, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          expected = [IdInt 0 0, IdInt 2 1, IdInt 3 3, IdInt 4 5, IdInt 1 3]
          idInt = idInts !! 1

      actual <- mixOneTest idInts idInt

      compareNumbers actual expected

    it "should mix one forward (3)" $ do
      let idInts = [IdInt 0 0, IdInt 1 15, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          expected = [IdInt 0 0, IdInt 2 1, IdInt 3 3, IdInt 4 5, IdInt 1 15]
          idInt = idInts !! 1

      actual <- mixOneTest idInts idInt

      compareNumbers actual expected

    it "should mix one backward (1)" $ do
      let idInts = [IdInt 0 0, IdInt 1 4, IdInt 2 1, IdInt 3 (-3), IdInt 4 5]
          idInt = IdInt 3 (-3)
          expected = [IdInt 3 (-3), IdInt 0 0, IdInt 1 4, IdInt 2 1, IdInt 4 5]

      actual <- mixOneTest idInts idInt

      compareNumbers actual expected

    it "should mix one backward (2)" $ do
      let idInts = [IdInt 0 0, IdInt 1 4, IdInt 2 1, IdInt 3 (-15), IdInt 4 5]
          idInt = IdInt 3 (-15)
          expected = [IdInt 3 (-15), IdInt 0 0, IdInt 1 4, IdInt 2 1, IdInt 4 5]

      actual <- mixOneTest idInts idInt

      compareNumbers actual expected

  describe "mix" $ do
    it "should mix a list of IdInt" $ do
      let idInts = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          expected = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

      actual <- mix idInts idInts

      compareNumbers actual expected
