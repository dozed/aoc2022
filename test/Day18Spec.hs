module Day18Spec where

import Test.Hspec

import Day18
import Util (regularParse)

day18Spec :: Spec
day18Spec = do

  describe "cubePositionsParser" $ do
    it "should parse a list of CubePos" $ do
      regularParse cubePositionsParser miniInput `shouldBe` Right [(1, 1, 1), (2, 1, 1)]
