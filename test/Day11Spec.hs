module Day11Spec (day11Spec) where

import Test.Hspec

import Day11
import Util (regularParse)

testMonkeys :: [Monkey]
testMonkeys = [
      Monkey {idx = 0, startingItems = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
      Monkey {idx = 1, startingItems = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
      Monkey {idx = 2, startingItems = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
      Monkey {idx = 3, startingItems = [74], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
    ]

day11Spec :: Spec
day11Spec = do
  describe "monkeysParser" $ do
    it "should parse Monkeys" $ do
      regularParse monkeysParser testInput1 `shouldBe` Right testMonkeys

  describe "updateItemWorryLevel" $ do
    it "should compute correct value" $ do
      updateItemWorryLevel (MulWith 2) 3 `shouldBe` 6
      updateItemWorryLevel SquareOld 3 `shouldBe` 9
      updateItemWorryLevel (AddWith 2) 3 `shouldBe` 5

  describe "chooseTarget" $ do
    let monkey = Monkey {idx = 0, startingItems = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3}

    it "should choose trueThrowTo target" $ do
      chooseTarget monkey 46 `shouldBe` 2

    it "should choose falseThrowTo target" $ do
      chooseTarget monkey 47 `shouldBe` 3

  describe "throwItem" $ do
    it "should move item from one monkey to another" $ do
      let expected = [
              Monkey {idx = 0, startingItems = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, startingItems = [54,65,75], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, startingItems = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, startingItems = [74,74], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      throwItem testMonkeys 3 1 3 `shouldBe` expected
