{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day17Spec (day17Spec) where

import qualified Data.Set as S
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

exampleField3 :: String
exampleField3 = strip [r|
|......#| <- i
|......#|
|...####|
|..##...|
|..#....|
|..#.#..|
|..###..|
|....#..|
|...###.|
|..#.#..|
|#######|
|###.#..|
|.##.#..|
|.####..|
|##.####|
+-------+
|]

day17Spec :: Spec
day17Spec = do

  describe "jetsParser" $ do
    it "should parse a list of jet descriptions" $ do
      regularParse jetsParser ">><" `shouldBe` Right [JetRight, JetRight, JetLeft]

  describe "readField" $ do
    it "should read a field from a string" $ do
      let pos = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                 (0, 1), (1, 1), (2, 1),
                 (2, 3), (3, 3),
                 (2, 4), (3, 4)]

      readField exampleField1 `shouldBe` S.fromList pos
