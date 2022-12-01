module Util where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import System.IO

readFileText :: String -> IO String
readFileText filename = do
  handle <- openFile filename ReadMode
  content <- hGetContents handle
  -- hClose handle
  pure content

readFileLines :: String -> IO [String]
readFileLines filename = do
  content <- readFileText filename
  let xs = words content
  pure xs

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
  where (lft, (_:rgt)) = splitAt idx xs

maxIndex :: Ord b => [b] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]
