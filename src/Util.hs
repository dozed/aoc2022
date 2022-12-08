module Util (
  readFileLines,
  count,
  windows,
  deleteAt,
  maxIndex,
  regularParse,
  intersect,
  replaceAtIndex
) where

import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Text.Parsec (ParseError)
import Text.ParserCombinators.Parsec (parse)
import Text.Parsec.String (Parser)

readFileLines :: String -> IO [String]
readFileLines filename = lines <$> readFile filename

count :: Eq a => a -> [a] -> Int
count x xs = length . filter (x ==) $ xs

windows :: Int -> [a] -> [[a]]
windows n0 = go 0 Seq.empty
  where
    go n s (a:as) | n' <  n0   =              go n' s'  as
                  | n' == n0   = toList s'  : go n' s'  as
                  | otherwise =  toList s'' : go n  s'' as
      where
        n'  = n + 1         -- O(1)
        s'  = s |> a        -- O(1)
        s'' = Seq.drop 1 s' -- O(1)
    go _ _ [] = []

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

maxIndex :: Ord b => [b] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

intersectSorted :: Ord a => [a] -> [a] -> [a]
intersectSorted (x:xs) (y:ys)
  | x == y = x : intersectSorted xs ys
  | x < y = intersectSorted xs (y:ys)
  | x > y = intersectSorted (x:xs) ys
intersectSorted _ _ = []

intersect :: Ord a => [a] -> [a] -> [a]
intersect xs ys = intersectSorted (sort xs) (sort ys)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs
