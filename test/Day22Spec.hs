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

  describe "getInternalPos" $ do
    it "should compute an internal Pos from an external Pos" $ do
      getInternalPos testExternalFieldPos 1 (12, 1) `shouldBe` (4, 1)
      getInternalPos testExternalFieldPos 3 (5, 5) `shouldBe` (1, 1)

  describe "getExternalPos" $ do
    it "should compute an external Pos from an internal Pos" $ do
      getExternalPos testExternalFieldPos 1 (4, 1) `shouldBe` (12, 1)
      getExternalPos testExternalFieldPos 3 (1, 1) `shouldBe` (5, 5)

  describe "getNew" $ do
    it "should compute a new row/column" $ do
      getNew TakeOne (2, 3) 4 `shouldBe` 1
      getNew TakeSize (2, 3) 4 `shouldBe` 4
      getNew TakeColumn (2, 3) 4 `shouldBe` 2
      getNew TakeRow (2, 3) 4 `shouldBe` 3
      getNew FlipColumn (2, 3) 4 `shouldBe` 3
      getNew FlipRow (2, 3) 4 `shouldBe` 2
