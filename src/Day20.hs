{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day20 where

import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust)
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ

import Util (lstrip, regularParse, removeAtIndex, insertAtIndex)

testInput :: String
testInput = lstrip [r|
1
2
-3
3
-2
0
4
|]

parseNumbers :: Parser [Int]
parseNumbers = endBy1 int endOfLine

type Id = Int
data IdInt = IdInt Id Int deriving (Eq, Show)

type Index = Int

getPos :: [IdInt] -> Id -> Index
getPos xs i = fromJust . findIndex (\(IdInt j _) -> i == j) $ xs

type Offset = Int

-- len: 8
-- pos: 2
--
-- 0 1 2 3 4 5 6 7
--     ^
--
-- offset: 7
-- pos': 1 = (2 + 7) mod 8
--
-- offset: -3
-- pos': 7 = (2 + (8 - (3 mod 8))) = (2 + 5)
--
-- offset: -13
-- pos': 5 = (2 + (8 - (13 mod 8))) = (2 + (8 - 5)) = (2 + 3)
applyOffset :: Int -> Index -> Offset -> Index
applyOffset len pos offset
  | offset >= 0 = (pos + offset) `mod` len
  | otherwise = (pos + (len - (abs offset `mod` len))) `mod` len

-- Moves an element a from a specific index to another index in a list
moveElement :: Index -> Index -> [a] -> [a]
moveElement from to xs =
  let x = xs !! from
      xs' = removeAtIndex from xs
      xs'' = insertAtIndex to x xs'
  in xs''

mix :: [IdInt] -> IdInt -> [IdInt]
mix xs el@(IdInt _ offset) =
  let from = fromJust . elemIndex el $ xs
      len = length xs
      to = applyOffset len from offset
      to' =
        if offset < 0 && to > from then to - 1
        else if offset > 0 && to < from then to + 1
        else if to == 0 then len - 1
        else to
      xs' = removeAtIndex from xs
      xs'' = insertAtIndex to' el xs'
  in xs''

day20 :: IO ()
day20 = do
  let input = testInput

  numbers <- case regularParse parseNumbers input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let idNumbers = zipWith IdInt [0..] numbers

  print numbers
  print idNumbers

  print $ getPos idNumbers 2
