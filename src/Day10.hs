{-# LANGUAGE QuasiQuotes #-}

module Day10 where

import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse)

testInput1 :: String
testInput1 = [r|noop
addx 3
addx -5
|]

data Op = AddX Int | Noop
          deriving (Eq, Show)

positiveNumberParser :: Parser Int
positiveNumberParser = read <$> many1 digit

negativeNumberParser :: Parser Int
negativeNumberParser = do
  void $ char '-'
  n <- positiveNumberParser
  return $ -n

numberParser :: Parser Int
numberParser = negativeNumberParser <|> positiveNumberParser

noopParser :: Parser Op
noopParser = Noop <$ try (string "noop")

addXParser :: Parser Op
addXParser = do
  void $ try (string "addx ")
  n <- numberParser
  return $ AddX n

opParser :: Parser Op
opParser = noopParser <|> addXParser

opsParser :: Parser [Op]
opsParser = endBy1 opParser endOfLine

getNumCycles :: Op -> Int
getNumCycles (AddX _) = 2
getNumCycles Noop = 1

data OpPhase = OpPhase Op Int

mkOpPhase :: Op -> OpPhase
mkOpPhase op = OpPhase op (getNumCycles op)

type Busy = Int
type Cycle = Int
type IntState = Int

runOp :: Op -> IntState -> IntState
runOp Noop i = i
runOp (AddX x) i = i + x

-- -- | loop nextOps currentOp busy cyc state
runOps :: [Op] -> Op -> Busy -> Cycle -> IntState -> (Cycle -> IntState -> IO()) -> IO ()
runOps [] currentOp 0 cyc state handleState = do
  -- putStrLn $ "[after " <> show (cyc-1) <> "] Op " <> show currentOp <> " finished"
  let newState = runOp currentOp state
  handleState cyc newState
  -- putStrLn $ "[after " <> show (cyc-1) <> "] program finished"
runOps (nextOp:otherOps) currentOp 0 cyc state handleState = do
  -- putStrLn $ "[after " <> show (cyc-1) <> "] Op " <> show currentOp <> " finished"
  let newState = runOp currentOp state
  handleState cyc newState
  let n = getNumCycles nextOp
  -- putStrLn $ "[begin " <> show cyc <> "] Started next op " <> show nextOp <> " with cycle length " <> show n
  runOps otherOps nextOp (n-1) (cyc+1) newState handleState
runOps nextOps currentOp busy cyc state handleState = do
  -- putStrLn $ "[while " <> show cyc <> "] Still running op " <> show currentOp
  handleState cyc state
  runOps nextOps currentOp (busy-1) (cyc+1) state handleState

type Probe = (Cycle, IntState)

makeProbesRef :: IO (IORef [Probe])
makeProbesRef = newIORef []

day10 :: IO ()
day10 = do
  -- let input = testInput1
  -- input <- readFile "input/Day10Test.txt"
  input <- readFile "input/Day10.txt"

  ops <- case regularParse opsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  print ops

  probesRef <- makeProbesRef

  let handleState c s = if c == 20 || (c - 20) `mod` 40 == 0 then do
                          putStrLn $ "[debug] cycle " <> show c <> " state: " <> show s
                          probes <- readIORef probesRef
                          writeIORef probesRef ((c, s) : probes)
                        else pure ()
  -- let handleState c s = putStrLn $ "[debug] cycle " <> show c <> " state: " <> show s
  let initialState = 1

  runOps (tail ops) (head ops) (getNumCycles $ head ops) 1 initialState handleState

  arr <- readIORef probesRef

  let signalStrengths = map (uncurry (*)) arr

  print $ sum signalStrengths
