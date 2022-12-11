{-# LANGUAGE QuasiQuotes #-}

module Day8 where

import Text.RawString.QQ

import Util (strip, takeUntil)

testInput :: String
testInput = [r|
30373
25512
65332
33549
35390
|]

type CellValue = Int
type Field = [[CellValue]]
type Rows = Int
type Cols = Int
type FieldSize = (Rows, Cols)
type Row = Int
type Col = Int
type CellIndex = (Row, Col)

readField :: String -> Field
readField = map (map (\x -> read [x])) . lines . strip

getSize :: Field -> FieldSize
getSize field =
  let rows = length field
      cols = length . head $ field
  in (rows, cols)

getCellIndices :: Field -> [CellIndex]
getCellIndices field =
  let (rows, cols) = getSize field
      cellIndices = [(row, col) | row <- [0..(rows-1)],
                                  col <- [0..(cols-1)]]
  in cellIndices

getCell :: CellIndex -> Field -> CellValue
getCell (row, col) field = (field !! row) !! col

getCells :: (CellIndex -> FieldSize -> [CellIndex]) -> CellIndex -> Field -> [CellValue]
getCells cellIdxsGen cellIdx field =
  let fieldSize = getSize field
      cellIdxs = cellIdxsGen cellIdx fieldSize
      cellValues = map (`getCell` field) cellIdxs
  in cellValues

getDownCells :: CellIndex -> Field -> [CellValue]
getDownCells = getCells (\(row, col) (rows, cols) -> [(row+1)..(rows-1)] `zip` repeat col)

getUpCells :: CellIndex -> Field -> [CellValue]
getUpCells = getCells (\(row, col) (rows, cols) -> reverse [0..(row-1)] `zip` repeat col)

getLeftCells :: CellIndex -> Field -> [CellValue]
getLeftCells = getCells (\(row, col) (rows, cols) -> repeat row `zip` reverse [0..(col-1)])

getRightCells :: CellIndex -> Field -> [CellValue]
getRightCells = getCells (\(row, col) (rows, cols) -> repeat row `zip` [(col+1)..(cols-1)])

isVisible :: CellIndex -> Field -> Bool
isVisible cellIdx field =
  let cellValue = getCell cellIdx field
      downCells = getDownCells cellIdx field
      upCells = getUpCells cellIdx field
      leftCells = getLeftCells cellIdx field
      rightCells = getRightCells cellIdx field
      visibleDown = all (< cellValue) downCells
      visibleUp = all (< cellValue) upCells
      visibleLeft = all (< cellValue) leftCells
      visibleRight = all (< cellValue) rightCells
      visible = visibleDown || visibleUp || visibleLeft || visibleRight
  in visible

getScenicScore :: CellIndex -> Field -> Int
getScenicScore cellIdx field =
  let cellValue = getCell cellIdx field
      downCells = getDownCells cellIdx field
      upCells = getUpCells cellIdx field
      leftCells = getLeftCells cellIdx field
      rightCells = getRightCells cellIdx field
      unblockedDownCells = length . takeUntil (< cellValue) $ downCells
      unblockedUpCells = length . takeUntil (< cellValue) $ upCells
      unblockedLeftCells = length . takeUntil (< cellValue) $ leftCells
      unblockedRightCells = length . takeUntil (< cellValue) $ rightCells
      scenicScore = unblockedDownCells * unblockedUpCells * unblockedLeftCells * unblockedRightCells
  in scenicScore

day8 :: IO ()
day8 = do
  -- let input = testInput
  input <- readFile "input/Day8.txt"

  let field = readField input

  -- part 1
  let numVisibles = length . filter (`isVisible` field) . getCellIndices $ field
  print numVisibles

  -- part 2
  let maxScenicScore = maximum . map (`getScenicScore` field) . getCellIndices $ field
  print maxScenicScore
