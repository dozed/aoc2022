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
