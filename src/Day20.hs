{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day20 where

import Control.Monad (foldM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, readArray, newListArray, getElems, getBounds)
import qualified Data.Ix as Ix
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

-- list + shift-based approach
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

-- STArray + shift-based approach
getLength :: STArray s Int a -> ST s Int
getLength arr = Ix.rangeSize <$> getBounds arr

elemIndex' :: Eq a => STArray s Int a -> a -> Index -> ST s (Maybe Int)
elemIndex' arr a i = do
  len <- getLength arr
  if i < len then do
    a' <- readArray arr i
    if a' == a then return (Just i) else elemIndex' arr a (i+1)
  else return Nothing

shift' :: Offset -> Index -> STArray s Int a -> ST s ()
shift' 0 _ _ = return ()
shift' offset from arr = do
  len <- getLength arr
  if len == 0 then return ()
  else do
    let to = if offset > 0 then
               if from == len - 1 then 0 else from + 1
             else
               if from == 0 then len - 1 else from - 1
        offset' = if offset > 0 then offset-1
                  else offset + 1
    swap' from to arr
    shift' offset' to arr

mixOne'' :: STArray s Int IdInt -> IdInt -> ST s ()
mixOne'' arr el@(IdInt _ offset) = do
  from <- fromJust <$> elemIndex' arr el 0
  shift' offset from arr
  return ()

mix'' :: [IdInt] -> ST s [IdInt]
mix'' idInts = do
  let len = length idInts
  arr <- newListArray (0, len-1) idInts
  forM_ idInts $ \i ->
    mixOne'' arr i
  getElems arr

day20 :: IO ()
day20 = do
  -- let input = testInput
  input <- readFile "input/Day20.txt"

  ints <- case regularParse parseInts input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let idInts = zipWith IdInt [0..] ints
      idInts' = runST $ mix'' idInts
      idInts'' = dropWhile (\(IdInt _ i) -> i /= 0) . cycle $ idInts'

  let (IdInt _ i) = idInts'' !! 1000
      (IdInt _ j) = idInts'' !! 2000
      (IdInt _ k) = idInts'' !! 3000

  putStrLn $ "i: " <> show i
  putStrLn $ "j: " <> show j
  putStrLn $ "k: " <> show k

  let groveCoordinates = i + j + k

  putStrLn $ "groveCoordinates: " <> show groveCoordinates
