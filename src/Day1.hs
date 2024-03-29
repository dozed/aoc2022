{-# LANGUAGE QuasiQuotes #-}

module Day1 (day1) where

import Data.List (groupBy, sort)
import Text.RawString.QQ

testInput1 :: String
testInput1 = [r|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
|]

testInput2 :: IO String
testInput2 = readFile "input/Day1.txt"

type ElfCaloriesDetailed = [Int]
type ElfCaloriesSummed = Int

parseElvesCaloriesDetailed :: String -> [ElfCaloriesDetailed]
parseElvesCaloriesDetailed str =
  let lined = lines str
      grouped = groupBy (\s1 s2 -> (not . null $ s1)  && (not . null $ s2)) lined
      filtered = filter (not . null . head) grouped
      mapped = map (map (\x -> read x :: Int)) filtered
  in mapped

sumCalories :: [ElfCaloriesDetailed] -> [ElfCaloriesSummed]
sumCalories = map sum

findNMaxCalories :: Int -> [ElfCaloriesSummed] -> [ElfCaloriesSummed]
findNMaxCalories n ecs = take n . reverse . sort $ ecs

day1 :: IO ()
day1 = do
  -- let txt = testInput1
  txt <- testInput2
  let ex1 = parseElvesCaloriesDetailed txt
  print ex1

  let summed = sumCalories ex1
  print summed

  let max1 = findNMaxCalories 3 summed
  print max1

  let sum1 = sum max1
  print sum1
