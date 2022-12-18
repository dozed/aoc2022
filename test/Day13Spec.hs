module Day13Spec (day13Spec) where

import Test.Hspec

import Day13
import Util (regularParse)

day13Spec :: Spec
day13Spec = do

  describe "packetParser" $ do
    it "should parse a packet" $ do
      regularParse packetParser "[1,1,3,1,1]" `shouldBe` Right (L [S 1, S 1, S 3, S 1, S 1])
      regularParse packetParser "[[1],[2,3,4]]" `shouldBe` Right (L [L [S 1], L [S 2, S 3, S 4]])
