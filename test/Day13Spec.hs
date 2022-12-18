{-# LANGUAGE QuasiQuotes #-}

module Day13Spec (day13Spec) where

import Text.RawString.QQ

import Test.Hspec

import Day13
import Util (lstrip, regularParse)

txt1 :: String
txt1 = lstrip [r|
[1,1,3,1,1]
[[1],[2,3,4]]
|]

txt2 :: String
txt2 = lstrip [r|
[1,1,3,1,1]
[[1],[2,3,4]]

[9]
[[8,7,6]]
|]

packet1 :: Packet
packet1 = L [S 1, S 1, S 3, S 1, S 1]

packet2 :: Packet
packet2 = L [L [S 1], L [S 2, S 3, S 4]]

packet3 :: Packet
packet3 = L [S 9]

packet4 :: Packet
packet4 = L [L [S 8, S 7, S 6]]

day13Spec :: Spec
day13Spec = do

  describe "packetParser" $ do
    it "should parse a packet" $ do
      regularParse packetParser "[1,1,3,1,1]" `shouldBe` Right packet1
      regularParse packetParser "[[1],[2,3,4]]" `shouldBe` Right packet2

  describe "packetPairParser" $ do
    it "should parse a packet pair" $ do
      regularParse packetPairParser txt1 `shouldBe` Right (packet1, packet2)

  describe "packetPairsParser" $ do
    it "should parse packet pairs" $ do
      regularParse packetPairsParser txt2 `shouldBe` Right [(packet1, packet2), (packet3, packet4)]

      let expected = [
              (L [S 1,S 1,S 3,S 1,S 1],L [S 1,S 1,S 5,S 1,S 1]),
              (L [L [S 1],L [S 2,S 3,S 4]],L [L [S 1],S 4]),
              (L [S 9],L [L [S 8,S 7,S 6]]),
              (L [L [S 4,S 4],S 4,S 4],L [L [S 4,S 4],S 4,S 4,S 4]),
              (L [S 7,S 7,S 7,S 7],L [S 7,S 7,S 7]),
              (L [],L [S 3]),
              (L [L [L []]],L [L []]),
              (L [S 1,L [S 2,L [S 3,L [S 4,L [S 5,S 6,S 7]]]],S 8,S 9],L [S 1,L [S 2,L [S 3,L [S 4,L [S 5,S 6,S 0]]]],S 8,S 9])
            ]

      regularParse packetPairsParser testInput1 `shouldBe` Right expected

  describe "getPacketOrder" $ do
    it "should detect correct packet order for small packets" $ do
      let p1 = S 1
      let p2 = S 2

      getPacketOrder p1 p2 `shouldBe` CorrectOrder

    it "should detect incorrect packet order for small packets" $ do
      let p1 = S 2
      let p2 = S 1

      getPacketOrder p1 p2 `shouldBe` IncorrectOrder

    it "should detect inconclusive packet order for small packets" $ do
      let p1 = S 2
      let p2 = S 2

      getPacketOrder p1 p2 `shouldBe` InconclusiveOrder

    it "should detect correct packet order for mismatching packets" $ do
      let p1 = S 1
      let p2 = L [S 2, S 3]

      getPacketOrder p1 p2 `shouldBe` CorrectOrder

      let p3 = S 1
      let p4 = L [S 1, S 3]

      getPacketOrder p3 p4 `shouldBe` CorrectOrder

      let p5 = L [S 1, S 3]
      let p6 = S 2

      getPacketOrder p5 p6 `shouldBe` CorrectOrder

    it "should detect incorrect packet order for mismatching packets" $ do
      let p1 = S 2
      let p2 = L [S 1, S 3]

      getPacketOrder p1 p2 `shouldBe` IncorrectOrder

      let p3 = L [S 2, S 3]
      let p4 = S 1

      getPacketOrder p3 p4 `shouldBe` IncorrectOrder

      let p5 = L [S 1, S 3]
      let p6 = S 1

      getPacketOrder p5 p6 `shouldBe` IncorrectOrder

    it "should detect inconclusive packet order for mismatching packets" $ do
      let p1 = S 1
      let p2 = L [S 1]

      getPacketOrder p1 p2 `shouldBe` InconclusiveOrder

      let p3 = L [S 1]
      let p4 = S 1

      getPacketOrder p3 p4 `shouldBe` InconclusiveOrder

    it "should detect correct packet order for unbalanced large packets" $ do
      let p1 = L []
      let p2 = L [S 1, S 5]

      getPacketOrder p1 p2 `shouldBe` CorrectOrder

    it "should detect incorrect packet order for unbalanced large packets" $ do
      let p1 = L [S 1, S 5]
      let p2 = L []

      getPacketOrder p1 p2 `shouldBe` IncorrectOrder

    it "should detect inconclusive packet order for empty large packets" $ do
      let p1 = L []
      let p2 = L []

      getPacketOrder p1 p2 `shouldBe` InconclusiveOrder

    it "should detect correct packet order for large packets" $ do
      let p1 = L [S 1, S 2]
      let p2 = L [S 2, S 3]

      getPacketOrder p1 p2 `shouldBe` CorrectOrder

      let p3 = L [S 2, S 2]
      let p4 = L [S 2, S 3]

      getPacketOrder p3 p4 `shouldBe` CorrectOrder

    it "should detect incorrect packet order for large packets" $ do
      let p1 = L [S 2, S 3]
      let p2 = L [S 1, S 2]

      getPacketOrder p1 p2 `shouldBe` IncorrectOrder

      let p3 = L [S 2, S 3]
      let p4 = L [S 2, S 2]

      getPacketOrder p3 p4 `shouldBe` IncorrectOrder

    it "should detect correct packet order for nested packets" $ do
      let p1 = L [L [S 1, S 2], S 5]
      let p2 = L [L [S 2, S 3], S 4]

      getPacketOrder p1 p2 `shouldBe` CorrectOrder

      let p3 = L [L [S 2, S 2], S 2]
      let p4 = L [L [S 2, S 3], S 2]

      getPacketOrder p3 p4 `shouldBe` CorrectOrder

    it "should detect packet order for example input" $ do
      packetPairs <- case regularParse packetPairsParser testInput1 of
        Left e -> fail $ show e
        Right xs -> pure xs
    
      let orders = map (uncurry getPacketOrder) packetPairs
      
      orders `shouldBe` [
          CorrectOrder, CorrectOrder, IncorrectOrder, CorrectOrder,
          IncorrectOrder, CorrectOrder, IncorrectOrder, IncorrectOrder
        ]