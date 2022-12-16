{-# LANGUAGE QuasiQuotes #-}

module Day11 where

import Control.Monad (forM_, foldM)
import Data.List (intercalate, sort)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse, replaceAtIndex, removeAtIndex)

testInput1 :: String
testInput1 = [r|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
|]

data Operation = MulWith Integer
               | SquareOld
               | AddWith Integer
               deriving (Eq, Show)

type ItemWorryLevel = Integer
type ItemIndex = Int
type MonkeyIndex = Int

data Monkey = Monkey {
  idx :: MonkeyIndex,
  items :: [ItemWorryLevel],
  operation :: Operation,
  testDivisor :: Integer,
  trueThrowTo :: MonkeyIndex,
  falseThrowTo :: MonkeyIndex
} deriving (Eq, Show)

printMonkeys :: [Monkey] -> IO ()
printMonkeys monkeys = forM_ monkeys $ do \m -> putStrLn $ "Monkey " <> show (idx m) <> ": " <> intercalate ", " (map show . items $ m)

numberListParser :: Parser [Integer]
numberListParser = sepBy1 (read <$> many1 digit) (string ", ")

operationParser :: Parser Operation
operationParser =
      try (SquareOld <$ (char '*' >> char ' ' >> string "old"))
  <|> (char '*' >> char ' ' *> (MulWith . read <$> many1 digit))
  <|> (char '+' >> char ' ' *> (AddWith . read <$> many1 digit))

monkeyParser :: Parser Monkey
monkeyParser = do
  idx <- read <$> (string "Monkey " *> many1 digit <* string ":\n")
  startingItems <- string "  Starting items: " *> numberListParser <* char '\n'
  operation <- string "  Operation: new = old " *> operationParser <* char '\n'
  testDivisor <- string "  Test: divisible by " *> (read <$> many1 digit) <* char '\n'
  trueThrowTo <- string "    If true: throw to monkey " *> (read <$> many1 digit) <* char '\n'
  falseThrowTo <- string "    If false: throw to monkey " *> (read <$> many1 digit) <* char '\n'
  return Monkey {
    idx = idx,
    items = startingItems,
    operation = operation,
    testDivisor = testDivisor,
    trueThrowTo = trueThrowTo,
    falseThrowTo = falseThrowTo
  }

monkeysParser :: Parser [Monkey]
monkeysParser = sepBy1 monkeyParser (char '\n')

updateItemWorryLevel :: Operation -> ItemWorryLevel -> ItemWorryLevel
updateItemWorryLevel (MulWith x) old = old * x
updateItemWorryLevel SquareOld old = old * old
updateItemWorryLevel (AddWith x) old = old + x

chooseTarget :: Monkey -> ItemWorryLevel -> MonkeyIndex
chooseTarget m wl =
  if wl `mod` testDivisor m == 0 then trueThrowTo m
  else falseThrowTo m

throwItem :: MonkeyIndex -> ItemIndex -> MonkeyIndex -> [Monkey] -> [Monkey]
throwItem fromMonkeyIndex itemIndex toMonkeyIndex monkeys =
  let fromMonkey = monkeys !! fromMonkeyIndex
      toMonkey = monkeys !! toMonkeyIndex
      item = items fromMonkey !! itemIndex
      fromMonkey' = fromMonkey { items = removeAtIndex itemIndex (items fromMonkey) }
      toMonkey' = toMonkey { items = items toMonkey <> [item] }
      monkeys' = replaceAtIndex fromMonkeyIndex fromMonkey' monkeys
      monkeys'' = replaceAtIndex toMonkeyIndex toMonkey' monkeys'
  in monkeys''

updateItemWorryLevel' :: Monkey -> Monkey
updateItemWorryLevel' = undefined

type UpdateWorryLevelAfterInspection = Integer -> Integer

monkeyInspectAndThrowFirstItem :: UpdateWorryLevelAfterInspection -> [Monkey] -> MonkeyIndex -> [Monkey]
monkeyInspectAndThrowFirstItem updateWorryLevelAfterInspection monkeys fromMonkeyIndex  =
  let fromMonkey = monkeys !! fromMonkeyIndex
      monkeyItems = items fromMonkey
      -- fromMonkey' = fromMonkey { items = tail monkeyItems }
      -- monkey inspects item: worry level is increased
      itemWorryLevel = head monkeyItems
      updatedItemWorryLevel = updateItemWorryLevel (operation fromMonkey) itemWorryLevel
      -- after monkey inspected item: worry level is decreased
      boredItemWorryLevel = updateWorryLevelAfterInspection updatedItemWorryLevel
      -- monkey tests worry level
      toMonkeyIndex = chooseTarget fromMonkey boredItemWorryLevel
      toMonkey = monkeys !! toMonkeyIndex
      -- actually throw the item
      fromMonkey' = fromMonkey { items = tail monkeyItems }
      toMonkey' = toMonkey { items = items toMonkey <> [boredItemWorryLevel] }
      monkeys' = replaceAtIndex fromMonkeyIndex fromMonkey' monkeys
      monkeys'' = replaceAtIndex toMonkeyIndex toMonkey' monkeys'
  in monkeys''

type MonkeyStats = [Int]

monkeyBusiness :: MonkeyStats -> Int
monkeyBusiness stats =
  let stats' = reverse . sort $ stats
      a = head stats'
      b = head $ tail stats'
      mb = a * b
  in mb

printRound :: Int -> [Monkey] -> MonkeyStats -> IO ()
printRound i monkeys stats = do
  putStrLn $ "Monkeys after round " <> show i <> ":"
  -- printMonkeys monkeys
  putStrLn $ "Stats: " <> show stats
  putStrLn ""

monkeyTakeTurn :: UpdateWorryLevelAfterInspection -> ([Monkey], MonkeyStats) -> MonkeyIndex -> ([Monkey], MonkeyStats)
monkeyTakeTurn updateWorryLevelAfterInspection (monkeys, stats) monkeyIndex =
  let monkey = monkeys !! monkeyIndex
      monkeyItems = items monkey
      monkeysAfterTurn = foldl (\xs _ -> monkeyInspectAndThrowFirstItem updateWorryLevelAfterInspection xs monkeyIndex) monkeys [0..length monkeyItems-1]
      numInspections = (stats !! monkeyIndex) + length monkeyItems
      stats' = replaceAtIndex monkeyIndex numInspections stats
  in (monkeysAfterTurn, stats')

monkeysRound :: UpdateWorryLevelAfterInspection -> ([Monkey], MonkeyStats) -> ([Monkey], MonkeyStats)
monkeysRound updateWorryLevelAfterInspection (monkeys, stats) = foldl (monkeyTakeTurn updateWorryLevelAfterInspection) (monkeys, stats) [0..length monkeys-1]

monkeysRoundM :: UpdateWorryLevelAfterInspection -> ([Monkey], MonkeyStats) -> Int -> IO ([Monkey], MonkeyStats)
monkeysRoundM updateWorryLevelAfterInspection (monkeys, stats) i = do
  let (monkeys', stats') = monkeysRound updateWorryLevelAfterInspection (monkeys, stats)
  printRound i monkeys' stats'
  return (monkeys', stats')

day11 :: IO ()
day11 = do
  let input = testInput1
  -- input <- readFile "input/Day11.txt"

  monkeys <- case regularParse monkeysParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let stats = replicate (length monkeys) 0

  printRound 0 monkeys stats

  let div3 = (`div` 3)
  let div2 = (`div` 2)
  (monkeys', stats') <- foldM (monkeysRoundM id) (monkeys, stats) [1..10000]

  print $ monkeyBusiness stats'
  -- Monkeys after round 1000:
  -- Stats: [5204,4792,199,5192]
