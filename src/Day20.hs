{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day20 where

import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ

import Util (lstrip, regularParse)

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

type Pos = Int

getPos' :: [IdInt] -> Id -> Pos -> Pos
getPos' [] _ _ = undefined
getPos' ((IdInt j _):xs) i pos
  | j == i = pos
  | otherwise = getPos' xs i (pos+1)

getPos :: [IdInt] -> Id -> Pos
getPos xs i = getPos' xs i 0

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
