import Test.Hspec

import Day9Spec
import Day10Spec
import Day11Spec
import Day12Spec

main :: IO ()
main = hspec $ do
  describe "Day9" day9Spec
  describe "Day10" day10Spec
  describe "Day11" day11Spec
  describe "Day12" day12Spec
