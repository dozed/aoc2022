module Day14Spec (day14Spec) where

import Test.Hspec

import Day14
import Util (regularParse)

day14Spec :: Spec
day14Spec = do

  describe "posParser" $ do
    it "should parse a Pos" $ do
      regularParse posParser "498,4" `shouldBe` Right (498, 4)

  describe "lineParser" $ do
    it "should parse a list of Pos" $ do
      regularParse lineParser "498,4 -> 498,6 -> 496,6" `shouldBe` Right [(498, 4), (498, 6), (496, 6)]
