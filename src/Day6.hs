module Day6 where

import Data.List (nub, findIndex)

import Util (windows)

testInput1 :: String
testInput1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

testInput2 :: String
testInput2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"

testInput3 :: String
testInput3 = "nppdvjthqldpwncqszvftbrmjlhg"

testInput4 :: String
testInput4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

testInput5 :: String
testInput5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

isUnique :: String -> Bool
isUnique x =
  let l1 = length x
      l2 = length . nub $ x
  in l1 == l2

day6 :: IO ()
day6 = do
  -- let input = testInput2
  input <- readFile "input/Day6.txt"

  -- part 1
  let xs = windows 4 input

  pos <- case findIndex isUnique xs of
    Nothing -> fail "could not find unique pattern"
    Just x -> pure $ x + 4

  print pos

  -- part 2
  let xs = windows 14 input

  pos <- case findIndex isUnique xs of
    Nothing -> fail "could not find unique pattern"
    Just x -> pure $ x + 14

  print pos
