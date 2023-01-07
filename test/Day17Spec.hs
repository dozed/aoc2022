{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day17Spec (day17Spec) where

import Data.Bits
import Data.Word
import Text.RawString.QQ

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, elements)

import Day17
import Util (regularParse, strip)

instance Arbitrary Block where
  arbitrary = do
    elements [HLine, Plus, L, VLine, Square]

exampleField1 :: String
exampleField1 = strip [r|
|..##...|
|..##...|
|.......|
|###....|
|#######|
+-------+
|]

exampleField2 :: String
exampleField2 = strip [r|
|...#...|
|..###..|
|...#...|
|.......|
|.......|
|.......|
|###....|
|#######|
+-------+
|]

day17Spec :: Spec
day17Spec = do

  describe "jetsParser" $ do
    it "should parse a list of jet descriptions" $ do
      regularParse jetsParser ">><" `shouldBe` Right [JetRight, JetRight, JetLeft]
