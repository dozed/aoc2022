{-# LANGUAGE QuasiQuotes #-}

module Day14 where

import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (lstrip)
  
testInput1 :: String
testInput1 = lstrip [r|
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
|]

day14 :: IO ()
day14 = do
  putStrLn "day14"
