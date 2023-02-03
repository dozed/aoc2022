module Day20Spec (day20Spec) where

import Test.Hspec

import Day20
import Util (regularParse)

day20Spec :: Spec
day20Spec = do

  describe "parseNumbers" $ do
    it "should parse numbers" $ do
      numbers <- case regularParse parseNumbers testInput of
        Left e -> fail $ show e
        Right xs -> pure xs

      numbers `shouldBe` [1, 2, -3, 3, -2, 0, 4]

  describe "getPos" $ do
    it "should get position of number" $ do
      let idNumbers = [IdInt 2 0, IdInt 0 1, IdInt 1 2]

      getPos idNumbers 1 `shouldBe` 2

  describe "swap" $ do
    it "should swap two positions" $ do
      let xs = [0, 1, 2, 3, 4, 5]

      swap 2 4 xs `shouldBe` [0, 1, 4, 3, 2, 5]

  describe "applyOffset" $ do
    it "should apply an offset to a position in a cyclic list" $ do
      applyOffset 8 2 7 `shouldBe` 1
      applyOffset 8 2 (-3) `shouldBe` 7
      applyOffset 8 2 (-13) `shouldBe` 5
