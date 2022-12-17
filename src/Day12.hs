{-# LANGUAGE QuasiQuotes #-}

module Day12 where

import Control.Monad (forM_)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Text.RawString.QQ

import Util (strip)

testInput1 :: String
testInput1 = strip [r|
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
|]

type Cell = Char
type Field = [[Cell]]
type Y = Int
type X = Int
type Pos = (Y, X)

mkField :: String -> Field
mkField = lines

getPos :: Cell -> Field -> Pos
getPos cell field = getPos' 0 field
  where
    getPos' rowIdx (row:others) = 
      let idx = (\colIdx -> (rowIdx,colIdx)) <$> elemIndex cell row
      in fromMaybe (getPos' (rowIdx+1) others) idx
    getPos' _ [] = undefined

getStartPos :: Field -> Pos
getStartPos = getPos 'S'

getEndPos :: Field -> Pos
getEndPos = getPos 'E'

day12 :: IO ()
day12 = do
  let input = testInput1

  let field = mkField input

  forM_ field $ \l -> do
    putStrLn l
