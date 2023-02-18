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
