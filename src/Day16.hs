{-# LANGUAGE QuasiQuotes #-}

module Day16 where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (lstrip)

testInput :: String
testInput = lstrip [r|
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
|]

type Valve = String
type FlowRate = Int
data Desc = Desc Valve FlowRate [Valve]
            deriving (Eq, Show)

valveParser :: Parser Valve
valveParser = do
  a <- upper
  b <- upper
  return [a, b]

descParser :: Parser Desc
descParser = do
  void $ string "Valve "
  valve <- valveParser
  void $ string " has flow rate="
  flowRate <- read <$> many1 digit
  void $ string "; tunnels lead to valves "
  toValves <- sepBy1 valveParser (string ", ")
  return $ Desc valve flowRate toValves

