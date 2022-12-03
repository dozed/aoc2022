{-# LANGUAGE QuasiQuotes #-}

module Day3 where

import Control.Monad (forM_, join)
import Data.List (sort, sortBy, nub)
import Data.Set (Set, unions)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ

testInput1 :: String
testInput1 = [r|
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
type ElfRucksack = ([ItemType], [ItemType])

parseInput :: String -> [ElfRucksack]
parseInput = map parseInputLine . filter (not . null) . lines

parseInputLine :: String -> ElfRucksack
parseInputLine s =
  let p = length s `div` 2
      (s1, s2) = splitAt p s
  in (s1, s2)

itemTypePriorities :: Map ItemType Int
itemTypePriorities = Map.fromList $ zip ['a'..'z'] [1..26] <> zip ['A'..'Z'] [27..56]

getItemTypePriority :: ItemType -> Int
getItemTypePriority c = itemTypePriorities Map.! c

getCommonItemTypes :: ElfRucksack -> [ItemType]
getCommonItemTypes (xs, ys) = nub . popItem [] $ (sort xs, sort ys)

popItem :: [ItemType] -> ElfRucksack -> [ItemType]
popItem commons ([], _) = commons
popItem commons (_, []) = commons
popItem commons (a:as, b:bs) | a == b = popItem (a:commons) (as, bs)
popItem commons (a:as, b:bs) | a < b = popItem commons (as, b:bs)
popItem commons (a:as, _:bs) = popItem commons (a:as, bs)

day3 :: IO ()
day3 = do
  -- let input = parseInput testInput1
  input <- parseInput <$> testInput2

  -- debug output
  forM_ input $ \(x, y) -> do
    print (sort x, sort y)
    let commons = getCommonItemTypes (x, y)
    print commons

  let totalPriority = sum . map getItemTypePriority . concatMap getCommonItemTypes $ input
  print totalPriority