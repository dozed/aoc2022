{-# LANGUAGE QuasiQuotes #-}

module Day14 where

import Control.Monad (forM_, join, void)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (lstrip, regularParse)

testInput1 :: String
testInput1 = lstrip [r|
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
|]

type X = Int
type Y = Int
type Pos = (X,Y)
type Path = [Pos]

posParser :: Parser Pos
posParser = do
  x <- read <$> many1 digit
  void $ char ','
  y <- read <$> many1 digit
  return (x, y)

pathParser :: Parser Path
pathParser = sepBy1 posParser (try (string " -> "))

pathsParser :: Parser [Path]
pathsParser = endBy1 pathParser endOfLine

day14 :: IO ()
day14 = do
  let input = testInput1

  paths <- case regularParse pathsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  forM_ paths print

  let minX = fst . minimumBy (compare `on` fst) $ join paths
      maxX = fst . maximumBy (compare `on` fst) $ join paths
      minY = snd . minimumBy (compare `on` snd) $ join paths
      maxY = snd . maximumBy (compare `on` snd) $ join paths
      width = maxX - minX
      height = maxY - minY

  putStrLn $ "minX: " <> show minX <> " maxX: " <> show maxX <> " minY: " <> show minY <> " maxY: " <> show maxY
  putStrLn $ "height: " <> show height <> " width: " <> show width 

  let paths' = map (map (\(x, y) -> (x - minX, y - minY))) paths
  
  forM_ paths' print
