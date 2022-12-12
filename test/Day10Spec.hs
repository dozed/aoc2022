{-# LANGUAGE QuasiQuotes #-}

module Day10Spec (day10Spec) where

import Test.Hspec

import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Text.RawString.QQ

import Day10
import Util (regularParse)

example1 :: String
example1 = [r|noop
addx 3
addx -5
|]

day10Spec :: Spec
day10Spec = do
  describe "opsParser" $ do
    it "should parse Ops" $ do
      regularParse opsParser example1 `shouldBe` Right [Noop, AddX 3, AddX (-5)]

    it "should parse more Ops" $ do
      txt <- liftIO $ readFile "input/Day10.txt"
      regularParse opsParser txt `shouldSatisfy` isRight
