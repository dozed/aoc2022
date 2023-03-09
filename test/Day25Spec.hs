module Day25Spec where

import Control.Monad (forM_)

import Test.Hspec

import Day25

check :: [(Integer, String)]
check = [
    (        1,              "1"),
    (        2,              "2"),
    (        3,             "1="),
    (        4,             "1-"),
    (        5,             "10"),
    (        6,             "11"),
    (        7,             "12"),
    (        8,             "2="),
    (        9,             "2-"),
    (       10,             "20"),
    (       15,            "1=0"),
    (       20,            "1-0"),
    (     2022,         "1=11-2"),
    (    12345,        "1-0---0"),
    (314159265,  "1121-1110-1=0")
  ]

day25Spec :: Spec
day25Spec = do

  describe "getReducedBase5" $ do
    it "should read a decimal to a reduced base-5 form" $ do
      forM_ check $ \(i, s) -> (showReducedBase5 . getReducedBase5 . getBase5) i `shouldBe` s

  describe "getDecimal" $ do
    it "should roundtrip" $ do
      forM_ check $ \(i, _) ->
        let r = getReducedBase5 . getBase5 $ i
            i' = getDecimal r
        in i' `shouldBe` i
