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
      getSidePos testExternalFieldPos 1 (12, 1) `shouldBe` (4, 1)
      getSidePos testExternalFieldPos 3 (5, 5) `shouldBe` (1, 1)

  describe "getFieldPos" $ do
    it "should compute a field Pos from a side Pos" $ do
      getFieldPos testExternalFieldPos 1 (4, 1) `shouldBe` (12, 1)
      getFieldPos testExternalFieldPos 3 (1, 1) `shouldBe` (5, 5)

  describe "getNew" $ do
    it "should compute a new row/column" $ do
      getNew TakeOne (2, 3) 4 `shouldBe` 1
      getNew TakeSize (2, 3) 4 `shouldBe` 4
      getNew TakeColumn (2, 3) 4 `shouldBe` 2
      getNew TakeRow (2, 3) 4 `shouldBe` 3
      getNew FlipColumn (2, 3) 4 `shouldBe` 3
      getNew FlipRow (2, 3) 4 `shouldBe` 2
