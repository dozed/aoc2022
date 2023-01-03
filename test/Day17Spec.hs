module Day17Spec (day17Spec) where

import Test.Hspec

import Day17
import Util (regularParse)

day17Spec :: Spec
day17Spec = do
  
  describe "jetsParser" $ do
    it "should parse a list of jet descriptions" $ do
      
      regularParse jetsParser ">><" `shouldBe` Right [JetRight, JetRight, JetLeft]
 