{-# LANGUAGE QuasiQuotes #-}

module Day13 where

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

-- [1,1,3,1,1]

packetParser :: Parser Packet
packetParser = undefined

day13 :: IO ()
day13 = do
  print "day13"
