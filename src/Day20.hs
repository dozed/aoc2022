{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day20 where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ

import Util (lstrip, move, regularParse, swap)

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

parseInts :: Parser [Int]
parseInts = endBy1 int endOfLine

type Id = Int
data IdInt = IdInt Id Int deriving (Eq, Show)

type Length = Int
type Index = Int
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
-- pos': 7 = (2 + (8 - (abs (-3) `mod` 8))) `mod` 8 = (2 + 5)
--
-- offset: -13
-- pos': 5 = (2 + (8 - (abs (-13) `mod` 8))) `mod` 8 = (2 + (8 - 5)) = (2 + 3)
applyOffset :: Length -> Index -> Offset -> Index
applyOffset len pos offset
  | offset >= 0 = (pos + offset) `mod` len
  | otherwise = (pos + (len - (abs offset `mod` len))) `mod` len

mixOne :: [IdInt] -> IdInt -> [IdInt]
mixOne xs el@(IdInt _ offset) =
  let from = fromJust . elemIndex el $ xs
      len = length xs
      to = applyOffset len from offset
      to' =
        if to == 0 then len - 1
        else if offset < 0 && to > from then to - 1
        else if offset > 0 && to < from then to + 1
        else to
      xs' = move from to' xs
  in xs'

mix :: [IdInt] -> [IdInt]
mix idInts = foldl mixOne idInts idInts

shift :: Offset -> Index -> [a] -> [a]
shift _ _ [] = []
shift 0 _ xs = xs
shift offset from xs | offset > 0 =
  let len = length xs
      to = if from == len - 1 then 0 else from + 1
      xs' = swap from to xs
      xs'' = shift (offset-1) to xs'
  in xs''
shift offset from xs =
  let len = length xs
      to = if from == 0 then len - 1 else from - 1
      xs' = swap from to xs
      xs'' = shift (offset+1) to xs'
  in xs''

day20 :: IO ()
day20 = do
  -- let input = testInput
  input <- readFile "input/Day20.txt"

  ints <- case regularParse parseInts input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let idInts = zipWith IdInt [0..] ints
      idInts' = mix idInts
      idInts'' = dropWhile (\(IdInt _ i) -> i /= 0) . cycle $ idInts'

  print $ take 10 ints
  print $ take 10 idInts
  print $ take 10 idInts'
  print $ take 10 idInts''

  let (IdInt _ i) = idInts'' !! 1000
      (IdInt _ j) = idInts'' !! 2000
      (IdInt _ k) = idInts'' !! 3000

  putStrLn $ "i: " <> show i
  putStrLn $ "j: " <> show j
  putStrLn $ "k: " <> show k

  let groveCoordinates = i + j + k

  putStrLn $ "groveCoordinates: " <> show groveCoordinates
