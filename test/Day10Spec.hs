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

  describe "runOp" $ do
    it "should keep state for Noop" $ do
      runOp Noop 10 `shouldBe` 10

    it "should modify state for AddX" $ do
      runOp (AddX 2) 10 `shouldBe` 12
      runOp (AddX (-3)) 10 `shouldBe` 7

  describe "getPixel" $ do
    it "should compute dark pixel" $ do
      getPixel 8 10 `shouldBe` '.' 
      getPixel 12 10 `shouldBe` '.' 

    it "should compute lit pixel" $ do
      getPixel 9 10 `shouldBe` '#' 
      getPixel 10 10 `shouldBe` '#' 
      getPixel 11 10 `shouldBe` '#' 
