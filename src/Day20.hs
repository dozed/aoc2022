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

import Util (lstrip, regularParse)
import qualified UtilVector as UV

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

intsParser :: Parser [Int]
intsParser = endBy1 int endOfLine

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

mixOne :: IOVector IdInt -> IdInt -> IO ()
mixOne vec el@(IdInt _ offset) = do
  from <- fromJust <$> UV.elemIndex vec el 0
  let len = MV.length vec
      offset' = getSmallOffset offset len
  UV.shift offset' from vec
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

  ints <- case regularParse intsParser input of
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

  mixed'' <- foldM (\current _ -> mix reference' current) reference' [1..10]

  let mixed''' = dropWhile (\(IdInt _ x) -> x /= 0) . cycle $ mixed''

  let (IdInt _ i) = mixed''' !! 1000
      (IdInt _ j) = mixed''' !! 2000
      (IdInt _ k) = mixed''' !! 3000

  putStrLn $ "i: " <> show i
  putStrLn $ "j: " <> show j
  putStrLn $ "k: " <> show k

  let groveCoordinates = i + j + k

  putStrLn $ "groveCoordinates: " <> show groveCoordinates
