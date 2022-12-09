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

type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]

type Item = Maybe Char

toStacks :: [[Item]] -> Stacks
toStacks = map catMaybes

data MoveSpec = MoveSpec {
  num :: Int,
  from :: Int,
  to :: Int
} deriving Show

emptyItemParser :: Parser Item
emptyItemParser = Nothing <$ try (string "   ")

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

stackIndexItemParser :: Parser Int
stackIndexItemParser = do
  void $ char ' '
  idx <- read <$> many1 digit
  void $ char ' '
  return idx

stackIndexParser :: Parser [Int]
stackIndexParser = sepBy1 stackIndexItemParser (char ' ')

moveSpecParser :: Parser MoveSpec
moveSpecParser = do
  void $ string "move "
  num <- read <$> many1 digit
  void $ string " from "
  from <- read <$> many1 digit
  void $ string " to "
  to <- read <$> many1 digit
  return MoveSpec { num = num, from = from, to = to }

inputParser :: Parser (Stacks, [Int], [MoveSpec])
inputParser = do
  void endOfLine
  items <- endBy1 itemsParser endOfLine
  idxs <- stackIndexParser
  void endOfLine
  void endOfLine
  specs <- endBy1 moveSpecParser endOfLine
  let items' = toStacks $ transpose items
  return (items', idxs, specs)

data Order = Reverse | Straight deriving Eq

moveCrates :: Order -> MoveSpec -> Stacks -> Stacks
moveCrates order (MoveSpec num from to) stacks =
  let from' = from - 1
      to' = to - 1
      fromStack = (stacks !! from')
      toStack = (stacks !! to')
      popped = (if order == Reverse then reverse else id) . take num $ fromStack
      fromStack' = drop num fromStack
      toStack' = popped ++ toStack
      stacks' = replaceAtIndex from' fromStack' stacks
      stacks'' = replaceAtIndex to' toStack' stacks'
  in stacks''

day5 :: IO ()
day5 = do
  -- let txt = testInput1
  txt <- readFile "input/Day5.txt"

  (stacks, moveSpecs) <- case regularParse inputParser txt of
    Left e -> fail $ show e
    Right (stacks, _, moveSpecs) -> pure (stacks, moveSpecs)

  print (stacks, moveSpecs)

  -- part 1
  let res = foldl (flip (moveCrates Reverse)) stacks moveSpecs
  print res

  let tops = map head res
  print tops

  -- part 2
  let res = foldl (flip (moveCrates Straight)) stacks moveSpecs
  print res

  let tops = map head res
  print tops
