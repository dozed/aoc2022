{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day20 where

import Control.Monad (foldM, forM_)
import Data.Array.IO (IOArray, readArray, newListArray)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ

import Util (lstrip, move, regularParse, swap, swap')

testInput :: String
testInput = lstrip [r|
1
2
-3
3
-2
0
4
|]

parseInts :: Parser [Int]
parseInts = endBy1 int endOfLine

type Id = Int
data IdInt = IdInt Id Int deriving (Eq, Show)

type Length = Int
type Index = Int
type Offset = Int

-- len: 8
-- pos: 2
--
-- 0 1 2 3 4 5 6 7
--     ^
--
-- offset: 7
-- pos': 1 = (2 + 7) mod 8
--
-- offset: -3
-- pos': 7 = (2 + (8 - (abs (-3) `mod` 8))) `mod` 8 = (2 + 5)
--
-- offset: -13
-- pos': 5 = (2 + (8 - (abs (-13) `mod` 8))) `mod` 8 = (2 + (8 - 5)) = (2 + 3)
applyOffset :: Length -> Index -> Offset -> Index
applyOffset len pos offset
  | offset >= 0 = (pos + offset) `mod` len
  | otherwise = (pos + (len - (abs offset `mod` len))) `mod` len

mixOne :: [IdInt] -> IdInt -> [IdInt]
mixOne xs el@(IdInt _ offset) =
  let from = fromJust . elemIndex el $ xs
      len = length xs
      to = applyOffset len from offset
      to' =
        if to == 0 then len - 1
        else if offset < 0 && to > from then to - 1
        else if offset > 0 && to < from then to + 1
        else to
      xs' = move from to' xs
  in xs'

mix :: [IdInt] -> [IdInt]
mix idInts = foldl mixOne idInts idInts

shift :: Offset -> Index -> [a] -> [a]
shift _ _ [] = []
shift 0 _ xs = xs
shift offset from xs | offset > 0 =
  let len = length xs
      to = if from == len - 1 then 0 else from + 1
      xs' = swap from to xs
      xs'' = shift (offset-1) to xs'
  in xs''
shift offset from xs =
  let len = length xs
      to = if from == 0 then len - 1 else from - 1
      xs' = swap from to xs
      xs'' = shift (offset+1) to xs'
  in xs''

mixOne' :: [IdInt] -> IdInt -> IO [IdInt]
mixOne' xs el@(IdInt _ offset) = do
  let from = fromJust . elemIndex el $ xs
      !xs' = shift offset from xs
  print el
  return xs'

mix' :: [IdInt] -> IO [IdInt]
mix' idInts = foldM mixOne' idInts idInts

-- IOArray-based solution
shift' :: Offset -> Index -> IOArray Int a -> Length -> IO ()
-- shift _ _ [] = return []
shift' 0 _ _ _ = return ()
shift' offset from arr len | offset > 0 = do
  let to = if from == len - 1 then 0 else from + 1
  swap' from to arr
  shift' (offset-1) to arr len
shift' offset from arr len = do
  let to = if from == 0 then len - 1 else from - 1
  swap' from to arr
  shift' (offset+1) to arr len

elemIndex' :: Eq a => IOArray Int a -> a -> Length -> Index -> IO (Maybe Int)
elemIndex' arr a len i | i == len = return Nothing
elemIndex' arr a len i = do
  a' <- readArray arr i
  if a' == a then return (Just i) else elemIndex' arr a len (i+1)

toList' :: IOArray Int a -> Length -> Index -> [a] -> IO [a]
toList' arr len i as | i == len = return $ reverse as
toList' arr len i as = do
  a' <- readArray arr i
  toList' arr len (i+1) (a':as)

mixOne'' :: IOArray Int IdInt -> Length -> IdInt -> IO ()
mixOne'' arr len el@(IdInt _ offset) = do
  from <- fromJust <$> elemIndex' arr el len 0
  shift' offset from arr len
  print el
  return ()

mix'' :: IOArray Int IdInt -> [IdInt] -> Length -> IO ()
mix'' arr idInts len = forM_ idInts $ \i ->
  mixOne'' arr len i

day20 :: IO ()
day20 = do
  -- let input = testInput
  input <- readFile "input/Day20.txt"

  ints <- case regularParse parseInts input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let idInts = zipWith IdInt [0..] ints
      len = length idInts

  arr <- newListArray (0, len) idInts
  mix'' arr idInts len

  idInts' <- toList' arr len 0 []

  let idInts'' = dropWhile (\(IdInt _ i) -> i /= 0) . cycle $ idInts'

  let (IdInt _ i) = idInts'' !! 1000
      (IdInt _ j) = idInts'' !! 2000
      (IdInt _ k) = idInts'' !! 3000

  putStrLn $ "i: " <> show i
  putStrLn $ "j: " <> show j
  putStrLn $ "k: " <> show k

  let groveCoordinates = i + j + k

  putStrLn $ "groveCoordinates: " <> show groveCoordinates
