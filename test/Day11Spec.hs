module Day11Spec (day11Spec) where

import Test.Hspec

import Day11
import Util (regularParse)

day11Spec :: Spec
day11Spec = do
  describe "monkeysParser" $ do
    it "should parse Monkeys" $ do
      let expectedMonkeys = [
              Monkey {idx = 0, items = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      regularParse monkeysParser testInput1 `shouldBe` Right expectedMonkeys

  describe "updateItemWorryLevel" $ do
    it "should compute correct value" $ do
      updateItemWorryLevel (MulWith 2) 3 `shouldBe` 6
      updateItemWorryLevel SquareOld 3 `shouldBe` 9
      updateItemWorryLevel (AddWith 2) 3 `shouldBe` 5

  describe "chooseTarget" $ do
    let monkey = Monkey {idx = 0, items = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3}

    it "should choose trueThrowTo target" $ do
      chooseTarget monkey 46 `shouldBe` 2

    it "should choose falseThrowTo target" $ do
      chooseTarget monkey 47 `shouldBe` 3

  describe "monkeyInspectAndThrowFirstItem" $ do
    it "should throw first item from monkey 0 to monkey 3" $ do
      let testMonkeys = [
              Monkey {idx = 0, items = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      let expectedMonkeys = [
              Monkey {idx = 0, items = [98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      monkeyInspectAndThrowFirstItem testMonkeys 0 `shouldBe` expectedMonkeys

    it "should throw first item from monkey 0 to monkey 3" $ do
      let testMonkeys = [
              Monkey {idx = 0, items = [98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      let expectedMonkeys = [
              Monkey {idx = 0, items = [], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      monkeyInspectAndThrowFirstItem testMonkeys 0 `shouldBe` expectedMonkeys

    it "should throw first item from monkey 1 to monkey 0" $ do
      let testMonkeys = [
              Monkey {idx = 0, items = [], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      let expectedMonkeys = [
              Monkey {idx = 0, items = [20], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      monkeyInspectAndThrowFirstItem testMonkeys 1 `shouldBe` expectedMonkeys

    it "should throw first item from monkey 1 to monkey 0" $ do
      let testMonkeys = [
              Monkey {idx = 0, items = [20], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      let expectedMonkeys = [
              Monkey {idx = 0, items = [20,23], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      monkeyInspectAndThrowFirstItem testMonkeys 1 `shouldBe` expectedMonkeys

    it "should throw first item from monkey 1 to monkey 0" $ do
      let testMonkeys = [
              Monkey {idx = 0, items = [20,23], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      let expectedMonkeys = [
              Monkey {idx = 0, items = [20,23,27], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      monkeyInspectAndThrowFirstItem testMonkeys 1 `shouldBe` expectedMonkeys

    it "should throw first item from monkey 1 to monkey 0" $ do
      let testMonkeys = [
              Monkey {idx = 0, items = [20,23,27], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      let expectedMonkeys = [
              Monkey {idx = 0, items = [20,23,27,26], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74,500,620], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      monkeyInspectAndThrowFirstItem testMonkeys 1 `shouldBe` expectedMonkeys

  describe "monkeysRound" $ do
    it "should take a full round" $ do
      let testMonkeys = [
              Monkey {idx = 0, items = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      let expectedMonkeys = [
              Monkey {idx = 0, items = [20,23,27,26], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [2080,25,167,207,401,1046], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      monkeysRound testMonkeys `shouldBe` expectedMonkeys

  describe "getNumberOfInspectedItems" $ do
    it "should compute the number of inspected items" $ do
      let testMonkeys = [
              Monkey {idx = 0, items = [79,98], operation = MulWith 19, testDivisor = 23, trueThrowTo = 2, falseThrowTo = 3},
              Monkey {idx = 1, items = [54,65,75,74], operation = AddWith 6, testDivisor = 19, trueThrowTo = 2, falseThrowTo = 0},
              Monkey {idx = 2, items = [79,60,97], operation = SquareOld, testDivisor = 13, trueThrowTo = 1, falseThrowTo = 3},
              Monkey {idx = 3, items = [74], operation = AddWith 3, testDivisor = 17, trueThrowTo = 0, falseThrowTo = 1}
            ]

      getNumberOfInspectedItems testMonkeys `shouldBe` [2, 4, 3, 1]
