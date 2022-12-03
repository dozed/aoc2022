{-# LANGUAGE QuasiQuotes #-}

module Day3 where

import Combinatorics (tuples)
import Control.Monad (forM_, join)
import Data.List (sort, sortBy, nub)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ

import Util (intersect)

testInput1 :: String
testInput1 = [r|
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
|]

testInput1b :: String
testInput1b = [r|
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
|]

testInput2 :: IO String
testInput2 = readFile "input/Day3.txt"

type ItemType = Char
type ElfRucksackPartitioned = ([ItemType], [ItemType])
type ElfRucksackComplete = [ItemType]
type ElfGroup = [ElfRucksackComplete]

parseInput :: String -> [ElfRucksackPartitioned]
parseInput = map parseInputLine . filter (not . null) . lines

parseInputLine :: String -> ElfRucksackPartitioned
parseInputLine s =
  let p = length s `div` 2
      (s1, s2) = splitAt p s
  in (s1, s2)

parseElfGroups :: String -> [ElfGroup]
parseElfGroups s =
  let partitionedRucksacks = parseInput s
      completeRucksacks = map toCompleteRucksack partitionedRucksacks
      groups = chunksOf 3 completeRucksacks
  in groups

toCompleteRucksack :: ElfRucksackPartitioned -> ElfRucksackComplete
toCompleteRucksack (up, down) = up <> down

itemTypePriorities :: Map ItemType Int
itemTypePriorities = Map.fromList $ zip ['a'..'z'] [1..26] <> zip ['A'..'Z'] [27..56]

getItemTypePriority :: ItemType -> Int
getItemTypePriority c = itemTypePriorities Map.! c

getCommonItemTypes :: ElfRucksackPartitioned -> [ItemType]
getCommonItemTypes (xs, ys) = nub $ intersect xs ys

getCommonItemTypes' :: ElfGroup -> ItemType
getCommonItemTypes' xs =
  let pairs = tuples 2 xs
      commonInPairs = map (\x -> head x `intersect` head (tail x)) pairs
      commonInTriple = head commonInPairs `intersect` head (tail commonInPairs)
  in head commonInTriple

day3 :: IO ()
day3 = do
  -- part a
  -- let inputA = parseInput testInput1
  inputText <- testInput2
  let input = parseInput inputText

  -- debug output
  forM_ input $ \(x, y) -> do
    print (sort x, sort y)
    let commons = getCommonItemTypes (x, y)
    print commons
  print "---"

  let totalPriority = sum . map getItemTypePriority . concatMap getCommonItemTypes $ input
  print totalPriority

  -- part b
  let inputB = parseElfGroups inputText
  forM_ inputB print
  print "---"

  let totalScore = sum . map (getItemTypePriority . getCommonItemTypes') $ inputB
  print totalScore
