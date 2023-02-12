{-# LANGUAGE QuasiQuotes #-}

module Day20 where

import Control.Monad (foldM, forM_)
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import Data.Maybe (fromJust)
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ

import Util (lstrip, regularParse, swap')

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

cycleResetPrint :: [IdInt] -> IO ()
cycleResetPrint xs = do
  let len = length xs
      xs' = take len . dropWhile (\(IdInt _ x) -> x /= 0) . cycle $ xs
  print xs'

getSmallOffset :: Offset -> Length -> Offset
getSmallOffset offset len
  | offset >= 0 = (offset + (offset `div` (len-1))) `mod` len
  | otherwise   = -((abs offset + (abs offset `div` (len-1))) `mod` len)

shift :: Offset -> Index -> IOVector a -> IO ()
shift 0 _ _ = return ()
shift offset from vec = do
  let len = MV.length vec
      to = if offset > 0 then
             if from == len - 1 then 0 else from + 1
           else
             if from == 0 then len - 1 else from - 1
      offset' = if offset > 0 then offset - 1
                else offset + 1
  swap' from to vec
  shift offset' to vec

elemIndex :: Eq a => IOVector a -> a -> Index -> IO (Maybe Int)
elemIndex vec _ i | i == MV.length vec = return Nothing
elemIndex vec a i = do
  a' <- MV.read vec i
  if a' == a then return (Just i)
  else elemIndex vec a (i+1)

mixOne :: IOVector IdInt -> IdInt -> IO ()
mixOne vec el@(IdInt _ offset) = do
  from <- fromJust <$> elemIndex vec el 0
  let len = MV.length vec
      offset' = getSmallOffset offset len
  shift offset' from vec
  print el
  return ()

mix :: [IdInt] -> [IdInt] -> IO [IdInt]
mix reference current = do
  vec <- V.thaw $ V.fromList current
  forM_ reference $ \i -> do
    mixOne vec i
  vec' <- V.freeze vec
  let xs = V.toList vec'
  cycleResetPrint xs
  return xs

day20 :: IO ()
day20 = do
  -- let input = testInput
  input <- readFile "input/Day20.txt"

  ints <- case regularParse parseInts input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let reference = zipWith IdInt [0..] ints

  -- part 1
  putStrLn "--- part 1 ---"
  mixed <- mix reference reference

  let mixed' = dropWhile (\(IdInt _ x) -> x /= 0) . cycle $ mixed

  let (IdInt _ i) = mixed' !! 1000
      (IdInt _ j) = mixed' !! 2000
      (IdInt _ k) = mixed' !! 3000

  putStrLn $ "i: " <> show i
  putStrLn $ "j: " <> show j
  putStrLn $ "k: " <> show k

  let groveCoordinates = i + j + k

  putStrLn $ "groveCoordinates: " <> show groveCoordinates

  -- part 2
  putStrLn "--- part 2 ---"
  let decryptionKey = 811589153
      reference' = map (\(IdInt i x) -> IdInt i (x * decryptionKey)) reference

  idInts''' <- foldM (\current _ -> mix reference' current) reference' [1..10]

  let idInts'''' = dropWhile (\(IdInt _ x) -> x /= 0) . cycle $ idInts'''

  let (IdInt _ i) = idInts'''' !! 1000
      (IdInt _ j) = idInts'''' !! 2000
      (IdInt _ k) = idInts'''' !! 3000

  putStrLn $ "i: " <> show i
  putStrLn $ "j: " <> show j
  putStrLn $ "k: " <> show k

  let groveCoordinates = i + j + k

  putStrLn $ "groveCoordinates: " <> show groveCoordinates
