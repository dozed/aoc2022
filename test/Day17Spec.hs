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

exampleField4 :: String
exampleField4 = [r|
|..#....|
|.###...|
|..#....|
|.####..|
|....##.|
|....##.|
|....#..|
|..#.#..|
|..#.#..|
|#####..|
|..###..|
|...#...|
|..####.|
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

  describe "isAdjacent" $ do
    it "should detect adjacent positions" $ do
      isAdjacent (0, 0) (1, 0) `shouldBe` True
      isAdjacent (0, 0) (0, 1) `shouldBe` True

    it "should detect non-adjacent positions" $ do
      isAdjacent (0, 0) (1, 1) `shouldBe` False
      isAdjacent (0, 0) (0, 2) `shouldBe` False
      isAdjacent (0, 0) (2, 0) `shouldBe` False

  describe "getMaxY" $ do
    it "should get the maximum y position of a field" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1),
                              (2, 3), (3, 3),
                              (2, 4), (3, 4)]

      getMaxY field `shouldBe` 4

  describe "getDownPos" $ it "should get position below" $ getDownPos (2, 1) `shouldBe` (2, 0)
  describe "getUpPos" $ it "should get position above" $ getUpPos (2, 1) `shouldBe` (2, 2)
  describe "getRightPos" $ it "should get position to the right" $ getRightPos (2, 1) `shouldBe` (3, 1)
  describe "getLeftPos" $ it "should get position to the left" $ getLeftPos (2, 1) `shouldBe` (1, 1)

  describe "isBlockedByWall" $ do
    it "should detect that a position is out of the playfield" $ do
      isBlockedByWall (-1, 0) `shouldBe` True
      isBlockedByWall (7, 0) `shouldBe` True
      isBlockedByWall (0, -1) `shouldBe` True

    it "should detect that a position is inside the playfield" $ do
      isBlockedByWall (0, 0) `shouldBe` False
      isBlockedByWall (6, 0) `shouldBe` False
      isBlockedByWall (0, 1) `shouldBe` False

  describe "isBlockedByFieldRock" $ do
    it "should detect that a position is blocked by a rock" $ isBlockedByFieldRock (readField exampleField1) (2, 1) `shouldBe` True
    it "should detect that a position is not blocked by a rock" $ isBlockedByFieldRock (readField exampleField1) (3, 1) `shouldBe` False

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

  describe "getFreePosInRow" $ do
    it "should get free positions in a row of a field" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1)]

      getFreePosInRow field 1 `shouldBe` [(3, 1), (4, 1), (5, 1), (6, 1)]

  describe "getRockPosInRow" $ do
    it "should get rock positions in a row of a field" $ do
      let field = S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0),
                              (0, 1), (1, 1), (2, 1)]

      getRockPosInRow field 1 `shouldBe` [(0, 1), (1, 1), (2, 1)]

  describe "getReachables" $ do
    it "should get reachable positions" $ do
      let field = readField exampleField3
          expected = S.fromList [(0, 8), (1, 8), (5, 8), (6, 8),
                                 (0, 7), (1, 7), (2, 7), (3, 7), (5, 7), (6, 7),
                                 (0, 6), (1, 6), (2, 6), (6, 6),
                                 (0, 5), (1, 5), (5, 5), (6, 5)
                                ]

      getReachables field `shouldBe` expected

  describe "traceWaveFront" $ do
    it "should return the current wave front of the field" $ do
      let field = readField exampleField3
          expected = S.fromList [(2, 8), (3, 8), (4, 8),
                                 (4, 7),
                                 (3, 6), (5, 6),
                                 (2, 5), (4, 5),
                                 (0, 4), (1, 4), (5, 4), (6, 4)
                                ]

      traceWaveFront field `shouldBe` expected
