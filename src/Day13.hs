{-# LANGUAGE QuasiQuotes #-}

module Day13 where

import Control.Monad (forM_, void)
import Data.List (elemIndex, sortBy)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (catPairs, lstrip, regularParse)

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

data PacketOrder = CorrectOrder | IncorrectOrder | InconclusiveOrder
                   deriving (Eq, Show)

isCorrectOrder :: PacketOrder -> Bool
isCorrectOrder CorrectOrder = True
isCorrectOrder _ = False

toOrdering :: PacketOrder -> Ordering
toOrdering CorrectOrder = LT
toOrdering IncorrectOrder = GT
toOrdering InconclusiveOrder = EQ

getPacketOrder :: Packet -> Packet -> PacketOrder
getPacketOrder (S i) (S j)
  | i < j = CorrectOrder
  | i > j = IncorrectOrder
  | otherwise = InconclusiveOrder
getPacketOrder (S i) (L ys) = getPacketOrder (L [S i]) (L ys)
getPacketOrder (L xs) (S j) = getPacketOrder (L xs) (L [S j])
getPacketOrder (L []) (L (_:_)) = CorrectOrder
getPacketOrder (L (_:_)) (L []) = IncorrectOrder
getPacketOrder (L []) (L []) = InconclusiveOrder
getPacketOrder (L (x:xs)) (L (y:ys)) =
  case getPacketOrder x y of
    InconclusiveOrder -> getPacketOrder (L xs) (L ys)
    other -> other

smallPacketParser :: Parser Packet
smallPacketParser = S . read <$> many1 digit

largePacketParser :: Parser Packet
largePacketParser = do
  void $ char '['
  packets <- sepBy packetParser (char ',')
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
  -- let input = testInput1
  input <- readFile "input/Day13.txt"

  packetPairs <- case regularParse packetPairsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  print packetPairs

  forM_ packetPairs $ \(a, b) -> do
    putStrLn $ show (a, b) <> " " <> show (getPacketOrder a b)

  -- part 1
  let orders = map (uncurry getPacketOrder) packetPairs
      indexSum = sum . map snd . filter (\(o, i) -> isCorrectOrder o) $ orders `zip` [1..]

  putStrLn $ "No. correct orders: " <> show indexSum

  -- part 2
  let dividerPacket1 = L [L [S 2]]
      dividerPacket2 = L [L [S 6]]
      unsortedPackets = dividerPacket1 : dividerPacket2 : catPairs packetPairs
      sortedPackets = sortBy (\a b -> toOrdering (getPacketOrder a b)) unsortedPackets

  idx1 <- case elemIndex dividerPacket1 sortedPackets of
    Nothing -> fail "Could not find dividerPacket1"
    Just x -> pure $ x + 1

  idx2 <- case elemIndex dividerPacket2 sortedPackets of
    Nothing -> fail "Could not find dividerPacket2"
    Just x -> pure $ x + 1

  putStrLn $ "Decoder key: " <> show (idx1 * idx2)
