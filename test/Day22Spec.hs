module Day22Spec where

import Test.Hspec

import Day22

day22Spec :: Spec
day22Spec = do

  describe "parseMove" $ do
    it "should parse a move string" $ do
      parseMove "L" `shouldBe` TurnLeft
      parseMove "R" `shouldBe` TurnRight
      parseMove "10" `shouldBe` MoveForward 10

  describe "parseMoves" $ do
    it "should parse a moves string" $ do
      let expected = [
              MoveForward 10, TurnRight, MoveForward 5, TurnLeft, MoveForward 5, TurnRight, MoveForward 10, TurnLeft,
              MoveForward 4, TurnRight, MoveForward 5, TurnLeft, MoveForward 5
            ]
      parseMoves "10R5L5R10L4R5L5" `shouldBe` expected

  describe "getNextPos" $ do
    it "should compute the next Pos for a given Pos and Orientation" $ do
      getNextPos (2, 2) D `shouldBe` (2, 3)
      getNextPos (2, 2) U `shouldBe` (2, 1)
      getNextPos (2, 2) L `shouldBe` (1, 2)
      getNextPos (2, 2) R `shouldBe` (3, 2)

  describe "isOutsideSide" $ do
    it "should check that a Pos is inside a side" $ do
      isOutsideSide 4 (1, 1) `shouldBe` False
      isOutsideSide 4 (2, 2) `shouldBe` False
      isOutsideSide 4 (4, 4) `shouldBe` False

    it "should check that a Pos is outside a side" $ do
      isOutsideSide 4 (0, 0) `shouldBe` True
      isOutsideSide 4 (7, 7) `shouldBe` True
      isOutsideSide 4 (5, 5) `shouldBe` True

  describe "getSidePos" $ do
    it "should compute a side Pos from a field Pos" $ do
      getSidePos testSideFieldPos 1 (12, 1) `shouldBe` (4, 1)
      getSidePos testSideFieldPos 3 (5, 5) `shouldBe` (1, 1)

  describe "getFieldPos" $ do
    it "should compute a field Pos from a side Pos" $ do
      getFieldPos testSideFieldPos 1 (4, 1) `shouldBe` (12, 1)
      getFieldPos testSideFieldPos 3 (1, 1) `shouldBe` (5, 5)

  describe "getNew" $ do
    it "should compute a new row/column" $ do
      getNew TakeOne (2, 3) 4 `shouldBe` 1
      getNew TakeSize (2, 3) 4 `shouldBe` 4
      getNew TakeColumn (2, 3) 4 `shouldBe` 2
      getNew TakeRow (2, 3) 4 `shouldBe` 3
      getNew FlipColumn (2, 3) 4 `shouldBe` 3
      getNew FlipRow (2, 3) 4 `shouldBe` 2

  describe "go2" $ do
    it "should advance around the field" $ do
      let input = testInput
          fieldLines = init . init $ lines input
          field = readField fieldLines
          sideSize = 4
          connections = testConnections
          sideFieldPos = testSideFieldPos
          startSide = 1

      let (pos, orient, side) = go2 field sideSize connections sideFieldPos 1 (9, 1) R (MoveForward 10)
      pos `shouldBe` (11, 1)
      orient `shouldBe` R
      side `shouldBe` 1

      let (pos, orient, side) = go2 field sideSize connections sideFieldPos 1 (11, 1) D (MoveForward 5)
      pos `shouldBe` (11, 6)
      orient `shouldBe` D
      side `shouldBe` 4

      let (pos, orient, side) = go2 field sideSize connections sideFieldPos 4 (11, 6) R (MoveForward 5)
      pos `shouldBe` (15, 11)
      orient `shouldBe` D
      side `shouldBe` 6

      let (pos, orient, side) = go2 field sideSize connections sideFieldPos 6 (15, 11) L (MoveForward 10)
      pos `shouldBe` (11, 11)
      orient `shouldBe` L
      side `shouldBe` 5

      let (finalPos, finalOrient, finalSide) =
            foldl (\(pos, orient, side) move -> go2 field sideSize connections sideFieldPos side pos orient move)
                  ((9, 1), R, startSide) [MoveForward 10, TurnRight, MoveForward 5]
      finalPos `shouldBe` (11, 6)
      finalOrient `shouldBe` D
      finalSide `shouldBe` 4

      let (finalPos, finalOrient, finalSide) =
            foldl (\(pos, orient, side) move -> go2 field sideSize connections sideFieldPos side pos orient move)
                  ((9, 1), R, startSide) [MoveForward 10, TurnRight, MoveForward 5, TurnLeft, MoveForward 5]
      finalPos `shouldBe` (15, 11)
      finalOrient `shouldBe` D
      finalSide `shouldBe` 6

      let (finalPos, finalOrient, finalSide) =
            foldl (\(pos, orient, side) move -> go2 field sideSize connections sideFieldPos side pos orient move)
                  ((9, 1), R, startSide) [MoveForward 10, TurnRight, MoveForward 5, TurnLeft, MoveForward 5, TurnRight, MoveForward 10]
      finalPos `shouldBe` (11, 11)
      finalOrient `shouldBe` L
      finalSide `shouldBe` 5
