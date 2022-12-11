module Util (
  count,
  deleteAt,
  intersect,
  maxIndex,
  readFileLines,
  regularParse,
  replaceAtIndex,
  strip,
  takeUntil,
  windows
) where

import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Text.Parsec (ParseError, parse)
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

-- https://hackage.haskell.org/package/MissingH-1.0.0/docs/Data-String-Utils.html
wschars :: String
wschars = " \t\r\n"

{- | Removes any whitespace characters that are present at the start
or end of a string. Does not alter the internal contents of a
string. If no whitespace characters are present at the start or end
of a string, returns the original string unmodified. Safe to use on
any string.

Note that this may differ from some other similar
functions from other authors in that:

1. If multiple whitespace
characters are present all in a row, they are all removed;

2. If no
whitespace characters are present, nothing is done.
-}
strip :: String -> String
strip = lstrip . rstrip

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if x `elem` wschars
                            then lstrip xs
                            else s

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- https://stackoverflow.com/a/22472610/1590415
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then takeUntil p xs
                         else []
