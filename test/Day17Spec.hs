{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day17Spec (day17Spec) where

import qualified Data.Set as S
import Text.RawString.QQ

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, choose, elements, (==>))

import Day17
import Util (regularParse, strip)

instance Arbitrary Block where
  arbitrary = do
    elements [HLine, Plus, L, VLine, Square]

data FieldPosSpec = FieldPosSpec X Y
                    deriving (Show, Eq)

instance Arbitrary FieldPosSpec where
  arbitrary = do
    x <- choose (0, 6)
    y <- choose (0, 40)
    return $ FieldPosSpec x y

getPos :: FieldPosSpec -> Pos
getPos (FieldPosSpec x y) = (x, y)

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

    prop "should be the inverse of showField" $ \(pos :: [FieldPosSpec]) -> not (null pos) ==> do
      let pos' = S.fromList $ map getPos pos
      readField (showField pos') `shouldBe` pos'

  describe "showField" $ do
    it "should show a field" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1),
                              (2, 3), (3, 3),
                              (2, 4), (3, 4)]

      showField field `shouldBe` exampleField1

  describe "getMaxY" $ do
    it "should get the maximum y position of a field" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1),
                              (2, 3), (3, 3),
                              (2, 4), (3, 4)]

      getMaxY field `shouldBe` 4

  describe "getStartPos" $ do
    it "should get the starting position for a field" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1),
                              (2, 3), (3, 3),
                              (2, 4), (3, 4)]

      
      getStartPos field `shouldBe` (2, 8)

  describe "canMoveDown" $ do
    it "should detect that a block can move down" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1)]
      
      canMoveDown field Square (3, 2) `shouldBe` True

    it "should detect that a block can't move down" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1)]
      
      canMoveDown field Square (3, 1) `shouldBe` False

  describe "canMoveLeft" $ do
    it "should detect that a block can move left" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1)]
      
      canMoveLeft field Square (4, 1) `shouldBe` True

    it "should detect that a block can't move left" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1)]
      
      canMoveLeft field Square (3, 1) `shouldBe` False

  describe "canMoveRight" $ do
    it "should detect that a block can move right" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                                                              (4, 1), (5, 1), (6, 1)]
      
      canMoveRight field Square (1, 1) `shouldBe` True

    it "should detect that a block can't move right" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                                                              (4, 1), (5, 1), (6, 1)]
      
      canMoveRight field Square (2, 1) `shouldBe` False
