module Day11Spec (day11Spec) where

import Test.Hspec

import Day11
import Util (regularParse)

day11Spec :: Spec
day11Spec = do
  describe "monkeysParser" $ do
    it "should parse Monkeys" $ do
      let monkeys = [
              Monkey {idx = 0, startingItems = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, startingItems = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, startingItems = [79,60,97], operation = MulWithOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, startingItems = [74], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      regularParse monkeysParser testInput1 `shouldBe` Right monkeys
