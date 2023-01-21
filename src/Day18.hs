{-# LANGUAGE QuasiQuotes #-}

module Day18 where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (strip)

miniInput :: String
miniInput = strip [r|
1,1,1
2,1,1
|]

testInput :: String
testInput = strip [r|
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
|]

type X = Int
type Y = Int
type Z = Int

type CubePos = (X, Y, Z)

data Cube = Cube {
  cubeX :: X,
  cubeY :: Y,
  cubeZ :: Z
}

cubePositionParser :: Parser CubePos
cubePositionParser = do
  x <- read <$> many1 digit
  void $ char ','
  y <- read <$> many1 digit
  void $ char ','
  z <- read <$> many1 digit
  return (x, y, z)

cubePositionsParser :: Parser [CubePos]
cubePositionsParser = sepBy1 cubePositionParser endOfLine

day18 :: IO ()
day18 = do
  putStrLn "day 18"
