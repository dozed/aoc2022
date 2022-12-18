{-# LANGUAGE QuasiQuotes #-}

module Day13 where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (lstrip)
  
testInput1 :: String
testInput1 = lstrip [r|
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
|]

data Packet = L [Packet]
            | S Int
            deriving (Eq, Show)

smallPacketParser :: Parser Packet
smallPacketParser = S . read <$> many1 digit

largePacketParser :: Parser Packet
largePacketParser = do
  void $ char '['
  packets <- sepBy1 packetParser (char ',')
  void $ char ']'
  return $ L packets

packetParser :: Parser Packet
packetParser = largePacketParser <|> smallPacketParser

packetPairParser :: Parser (Packet, Packet)
packetPairParser = do
  p1 <- largePacketParser
  void endOfLine
  p2 <- largePacketParser
  void endOfLine
  return (p1, p2)

packetPairsParser :: Parser [(Packet, Packet)]
packetPairsParser = sepBy1 packetPairParser endOfLine 

day13 :: IO ()
day13 = do
  print "day13"
