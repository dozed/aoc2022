import Test.Hspec
import Test.DocTest

import UtilSpec
import UtilGraphSearchSpec
import UtilMatrixSpec
import UtilVectorSpec
import UtilViterbiSpec
import Day9Spec
import Day10Spec
import Day11Spec
import Day12Spec
import Day13Spec
import Day14Spec
import Day15Spec
import Day16Spec
import Day17Spec
import Day17bSpec
import Day18Spec
import Day19Spec
import Day20Spec
import Day21Spec
import Day22Spec

main :: IO ()
main = do
  doctest ["src/Day20.hs"]
  hspec $ do
    describe "Util" utilSpec
    describe "UtilGraphSearch" utilGraphSearchSpec
    describe "UtilMatrix" utilMatrixSpec
    describe "UtilVector" utilVectorSpec
    describe "UtilViterbi" utilViterbiSpec
    describe "Day9" day9Spec
    describe "Day10" day10Spec
    describe "Day11" day11Spec
    describe "Day12" day12Spec
    describe "Day13" day13Spec
    describe "Day14" day14Spec
    describe "Day15" day15Spec
    describe "Day16" day16Spec
    describe "Day17" day17Spec
    describe "Day17b" day17bSpec
    describe "Day18" day18Spec
    describe "Day19" day19Spec
    describe "Day20" day20Spec
    describe "Day21" day21Spec
    describe "Day22" day22Spec
