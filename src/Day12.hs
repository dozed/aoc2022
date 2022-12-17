{-# LANGUAGE QuasiQuotes #-}

module Day12 where

import Control.Applicative ((<|>))
import Control.Monad (forM_, mfilter)
import Data.Char (chr, ord)
import Data.List (elemIndex)
import Data.List.Extra ((!?))
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
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

getHeight :: Cell -> Cell
getHeight 'S' = 'a'
getHeight 'E' = 'z'
getHeight c = c

incrCell :: Cell -> Cell
incrCell c = chr (ord c + 1)

mkField :: String -> Field
mkField = lines

getPos :: Cell -> Field -> Maybe Pos
getPos cell field = getPos' 0 field
  where
    getPos' :: Y -> Field -> Maybe Pos
    getPos' rowIdx (row:others) =
      let idx = (\colIdx -> (rowIdx, colIdx)) <$> elemIndex cell row
      in idx <|> getPos' (rowIdx+1) others
    getPos' _ [] = Nothing

getStartPos :: Field -> Maybe Pos
getStartPos = getPos 'S'

getEndPos :: Field -> Maybe Pos
getEndPos = getPos 'E'

getCell :: Field -> Pos -> Maybe Cell
getCell field (y, x) = do
  row <- field !? y
  cell <- row !? x
  return cell

getAdjacentPositions :: Pos -> Set Pos
getAdjacentPositions (y, x) = S.fromList [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]

getReachablePositions :: Field -> Pos -> Set Pos
getReachablePositions field pos =
  case getCell field pos of
    Nothing -> S.empty
    Just currentCellValue ->
      let
        testCellValue = incrCell $ getHeight currentCellValue
        adjacentPositions = getAdjacentPositions pos
        isReachable pos = isJust . mfilter (<= testCellValue) . fmap getHeight . getCell field $ pos
        reachablePositions = S.filter isReachable adjacentPositions
      in reachablePositions

day12 :: IO ()
day12 = do
  let input = testInput1

  let field = mkField input

  startPos <- case getStartPos field of
    (Just pos) -> pure pos
    Nothing -> fail "could not get start position"

  endPos <- case getEndPos field of
    (Just pos) -> pure pos
    Nothing -> fail "could not get end position"

  -- debug
  putStrLn "Field:"
  forM_ field $ \l -> do
    putStrLn l

  putStrLn $ "startPos: " <> show startPos
  putStrLn $ "endPos: " <> show endPos

