import Test.Hspec

import Day9Spec
import Day10Spec
import Day11Spec
import Day12Spec
import Day13Spec
import Day14Spec
import Day15Spec

main :: IO ()
main = hspec $ do
  describe "Day9" day9Spec
  describe "Day10" day10Spec
  describe "Day11" day11Spec
  describe "Day12" day12Spec
  describe "Day13" day13Spec
  describe "Day14" day14Spec
  describe "Day15" day15Spec
