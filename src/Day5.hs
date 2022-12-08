{-# LANGUAGE QuasiQuotes #-}

module Day5 where

import Control.Monad (void)
import Data.List (transpose)
import Data.Maybe (isJust, catMaybes)
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.RawString.QQ

import Util (regularParse, replaceAtIndex)

testInput1 :: String
testInput1 = [r|
    [G] [R]                 [P]
    [H] [W]     [T] [P]     [H]
    [F] [T] [P] [B] [D]     [N]
[L] [T] [M] [Q] [L] [C]     [Z]
[C] [C] [N] [V] [S] [H]     [V] [G]
[G] [L] [F] [D] [M] [V] [T] [J] [H]
[M] [D] [J] [F] [F] [N] [C] [S] [F]
[Q] [R] [V] [J] [N] [R] [H] [G] [Z]|]

type Item = Maybe Char

emptyItemParser :: Parser Item
emptyItemParser = Nothing <$ string "   "

filledItemParser :: Parser Item
filledItemParser = do
  void $ char '['
  c <- anyChar
  void $ char ']'
  return $ Just c

itemParser :: Parser Item
itemParser = emptyItemParser <|> filledItemParser

lineParser :: Parser [Item]
lineParser = sepBy1 itemParser $ char ' '

inputParser :: Parser [[Item]]
inputParser = do
  void endOfLine
  sepBy1 lineParser endOfLine

type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]

toStacks :: [[Item]] -> Stacks
toStacks = map catMaybes

moveCrates :: Int -> Int -> Int -> Stacks -> Stacks
moveCrates num from to stacks =
  let fromStack = (stacks !! from)
      toStack = (stacks !! to)
      popped = reverse . take num $ fromStack
      fromStack' = drop num fromStack
      toStack' = popped ++ toStack
      stacks' = replaceAtIndex from fromStack' stacks
      stacks'' = replaceAtIndex to toStack' stacks'
  in stacks''

day5 :: IO ()
day5 = do
  print "day5"
  print $ regularParse lineParser "    [G] [R]                 [P]    "
  print $ regularParse inputParser testInput1

  let arr = [[Nothing,Just 'G',Just 'R',Nothing,Nothing,Nothing,Nothing,Just 'P'],[Nothing,Just 'H',Just 'W',Nothing,Just 'T',Just 'P',Nothing,Just 'H'],[Nothing,Just 'F',Just 'T',Just 'P',Just 'B',Just 'D',Nothing,Just 'N'],[Just 'L',Just 'T',Just 'M',Just 'Q',Just 'L',Just 'C',Nothing,Just 'Z'],[Just 'C',Just 'C',Just 'N',Just 'V',Just 'S',Just 'H',Nothing,Just 'V',Just 'G'],[Just 'G',Just 'L',Just 'F',Just 'D',Just 'M',Just 'V',Just 'T',Just 'J',Just 'H'],[Just 'M',Just 'D',Just 'J',Just 'F',Just 'F',Just 'N',Just 'C',Just 'S',Just 'F'],[Just 'Q',Just 'R',Just 'V',Just 'J',Just 'N',Just 'R',Just 'H',Just 'G',Just 'Z']]

  print arr
  print $ transpose arr
  print $ toStacks $ transpose arr
  
  let stacks = ["LCGMQ","GHFTCLDR","RWTMNFJV","PQVDFJ","TBLSMFN","PDCHVNR","TCH","PHNZVJSG","GHFZ"]
  print stacks

