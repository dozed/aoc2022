{-# LANGUAGE BangPatterns #-}

module Util (
  buildCombinations,
  buildPermutations,
  catPairs,
  count,
  deleteAt,
  dropUntil,
  findElem,
  filterNot,
  histogram,
  interleave,
  intersect,
  insertAtIndex,
  lstrip,
  maxIndex,
  move,
  readFileLines,
  removeAtIndex,
  regularParse,
  replaceAtIndex,
  rstrip,
  strip,
  swap,
  takeUntil,
  windows
) where

import Data.Foldable (toList)
import Data.List (sort)
import Data.List.HT (removeEach)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)

readFileLines :: String -> IO [String]
readFileLines filename = lines <$> readFile filename

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

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex i xs = take i xs ++ drop (i+1) xs

insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex i x xs = take i xs ++ [x] ++ drop i xs

-- Moves an element a from a specific index to another index in a list
move :: Int -> Int -> [a] -> [a]
move from to xs =
  let x = xs !! from
      xs' = removeAtIndex from xs
      xs'' = insertAtIndex to x xs'
  in xs''

swap :: Int -> Int -> [a] -> [a]
swap from to xs =
  let x = xs !! from
      y = xs !! to
      xs' = replaceAtIndex from y xs
      xs'' = replaceAtIndex to x xs'
  in xs''

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

-- https://hackage.haskell.org/package/yjtools-0.9.18/docs/Data-List-Tools.html#v:dropUntil
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ []     = []
dropUntil p (x:xs)
  | p x            = xs
  | otherwise      = dropUntil p xs

-- https://hackage.haskell.org/package/speculate-0.4.14/docs/src/Test.Speculate.Utils.Tuple.html#catPairs
catPairs :: [(a,a)] -> [a]
catPairs [] = []
catPairs ((x,y):xys) = x:y:catPairs xys

-- https://hackage.haskell.org/package/ghc-9.4.2/docs/src/GHC.Utils.Misc.html#count
count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs
                     
--count :: Eq a => a -> [a] -> Int
--count x xs = length . filter (x ==) $ xs

-- https://stackoverflow.com/a/55145433/1590415
histogram :: Ord a => [a] -> Map a Int
histogram = M.fromListWith (+) . (`zip` [1,1..])

buildCombinations :: Int -> Int -> [a] -> [[a]] -> [[a]]
buildCombinations maxLen 1 elems [] = buildCombinations maxLen 2 elems [[e] | e <- elems]
buildCombinations maxLen len elems combs =
  if len > maxLen then combs
  else
    let combs' = do
          perm <- combs
          el <- elems
          return $ el:perm
    in buildCombinations maxLen (len+1) elems combs'

buildPermutations :: [a] -> [[a]]
buildPermutations [] = [[]]
buildPermutations as = do
  (x, xs) <- removeEach as
  perm <- buildPermutations xs
  return $ x:perm

-- https://hackage.haskell.org/package/hxt-9.3.1.22/docs/src/Text.XML.HXT.Arrow.Pickle.Xml.html#findElem
findElem       :: (a -> Bool) -> [a] -> Maybe (a, [a])
findElem p     = find' id
    where
      find' _ []         = Nothing
      find' prefix (x : xs)
          | p x          = Just (x, prefix xs)
          | otherwise    = find' (prefix . (x:)) xs

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p as = filter (not . p) as

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave [] ys = ys
