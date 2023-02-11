{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Day20 where

import Control.Monad (foldM, forM_)
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
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

-- list/position-based approach
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

mix :: [IdInt] -> [IdInt] -> [IdInt]
mix reference current = foldl mixOne current reference

mixN :: Int -> [IdInt] -> [IdInt] -> [IdInt]
mixN n reference current = foldl mixOne current (take n reference)

-- list/shift-based approach
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

-- Vector/shift-based approach
shift' :: Offset -> Index -> IOVector a -> IO ()
shift' 0 _ _ = return ()
shift' offset from vec = do
  let len = MV.length vec
      to = if offset > 0 then
             if from == len - 1 then 0 else from + 1
           else
             if from == 0 then len - 1 else from - 1
      offset' = if offset > 0 then offset - 1
                else offset + 1
  swap' from to vec
  shift' offset' to vec

elemIndex' :: Eq a => IOVector a -> a -> Index -> IO (Maybe Int)
elemIndex' vec _ i | i == MV.length vec = return Nothing
elemIndex' vec a i = do
  a' <- MV.read vec i
  if a' == a then return (Just i)
  else elemIndex' vec a (i+1)

mixOne'' :: IOVector IdInt -> IdInt -> IO ()
mixOne'' vec el@(IdInt _ offset) = do
  from <- fromJust <$> elemIndex' vec el 0
  let len = MV.length vec
      offset' = if offset >= 0 then (offset + (offset `div` len)) `mod` len
                else -((abs offset + (abs offset `div` len)) `mod` len)
  shift' offset' from vec
  print el
  return ()

mix'' :: [IdInt] -> [IdInt] -> IO [IdInt]
mix'' reference current = do
  vec <- V.thaw $ V.fromList current
  forM_ reference $ \i -> do
    mixOne'' vec i
  vec' <- V.freeze vec
  let xs = V.toList vec'
  print xs
  return xs

mixN'' :: Int -> [IdInt] -> [IdInt] -> IO [IdInt]
mixN'' n reference current = do
  vec <- V.thaw $ V.fromList current
  forM_ (take n reference) $ \i ->
    mixOne'' vec i
  vec' <- V.freeze vec
  return $ V.toList vec'

day20 :: IO ()
day20 = do
  let input = testInput
  -- input <- readFile "input/Day20.txt"

  ints <- case regularParse parseInts input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let idInts = zipWith IdInt [0..] ints

  -- part 1
  putStrLn "--- part 1 ---"
  idInts' <- mix'' idInts idInts

  let idInts'' = dropWhile (\(IdInt _ i) -> i /= 0) . cycle $ idInts'

  let (IdInt _ i) = idInts'' !! 1000
      (IdInt _ j) = idInts'' !! 2000
      (IdInt _ k) = idInts'' !! 3000

  putStrLn $ "i: " <> show i
  putStrLn $ "j: " <> show j
  putStrLn $ "k: " <> show k

  let groveCoordinates = i + j + k

  putStrLn $ "groveCoordinates: " <> show groveCoordinates

  -- part 2
  putStrLn "--- part 2 ---"
  let decryptionKey = 811589153
      reference = map (\(IdInt i x) -> IdInt i (x * decryptionKey)) idInts

  idInts''' <- foldM (\current _ -> mix'' reference current) reference [1..10]
  
  let idInts'''' = dropWhile (\(IdInt _ i) -> i /= 0) . cycle $ idInts'''

  let (IdInt _ i) = idInts'''' !! 1000
      (IdInt _ j) = idInts'''' !! 2000
      (IdInt _ k) = idInts'''' !! 3000

  putStrLn $ "i: " <> show i
  putStrLn $ "j: " <> show j
  putStrLn $ "k: " <> show k

  let groveCoordinates = i + j + k

  putStrLn $ "groveCoordinates: " <> show groveCoordinates
