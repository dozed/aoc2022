{-# LANGUAGE QuasiQuotes #-}

module Day5 where

import Control.Monad (void)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse, replaceAtIndex)

testInput1 :: String
testInput1 = [r|
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|]

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

itemsParser :: Parser [Item]
itemsParser = sepBy1 itemParser $ char ' '

inputParser :: Parser [[Item]]
inputParser = do
  void endOfLine
  items <- try $ sepBy1 itemsParser endOfLine
  endOfLine
  many1 anyChar
  endOfLine
  endOfLine
  return items

type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]

toStacks :: [[Item]] -> Stacks
toStacks = map catMaybes

data MoveSpec = MoveSpec {
  num :: Int,
  from :: Int,
  to :: Int
}

moveCrates :: MoveSpec -> Stacks -> Stacks
moveCrates (MoveSpec num from to) stacks =
  let from' = from - 1
      to' = to - 1
      fromStack = (stacks !! from')
      toStack = (stacks !! to')
      popped = reverse . take num $ fromStack
      fromStack' = drop num fromStack
      toStack' = popped ++ toStack
      stacks' = replaceAtIndex from' fromStack' stacks
      stacks'' = replaceAtIndex to' toStack' stacks'
  in stacks''

day5 :: IO ()
day5 = do
  print "day5"
  print $ regularParse itemsParser "    [G] [R]                 [P]    "
  print $ regularParse itemsParser "    [D]    "
  print $ regularParse itemsParser "[N] [C]    "
  print $ regularParse inputParser testInput1

  let arr = [[Nothing,Just 'G',Just 'R',Nothing,Nothing,Nothing,Nothing,Just 'P'],[Nothing,Just 'H',Just 'W',Nothing,Just 'T',Just 'P',Nothing,Just 'H'],[Nothing,Just 'F',Just 'T',Just 'P',Just 'B',Just 'D',Nothing,Just 'N'],[Just 'L',Just 'T',Just 'M',Just 'Q',Just 'L',Just 'C',Nothing,Just 'Z'],[Just 'C',Just 'C',Just 'N',Just 'V',Just 'S',Just 'H',Nothing,Just 'V',Just 'G'],[Just 'G',Just 'L',Just 'F',Just 'D',Just 'M',Just 'V',Just 'T',Just 'J',Just 'H'],[Just 'M',Just 'D',Just 'J',Just 'F',Just 'F',Just 'N',Just 'C',Just 'S',Just 'F'],[Just 'Q',Just 'R',Just 'V',Just 'J',Just 'N',Just 'R',Just 'H',Just 'G',Just 'Z']]

  print arr
  print $ transpose arr
  print $ toStacks $ transpose arr

  let stacks = ["LCGMQ","GHFTCLDR","RWTMNFJV","PQVDFJ","TBLSMFN","PDCHVNR","TCH","PHNZVJSG","GHFZ"]
  print stacks
  print $ moveCrates (MoveSpec { num = 2, from = 1, to = 2 }) stacks

