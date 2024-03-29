{-# LANGUAGE QuasiQuotes #-}

module Day9 where

import Data.List (intercalate, nub, transpose)
import Data.Set (fromList, member)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse)

testInput1 :: String
testInput1 = [r|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
|]

testInput2 :: String
testInput2 = [r|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
|]

data MoveSpec = MoveUpSpec Int
              | MoveDownSpec Int
              | MoveLeftSpec Int
              | MoveRightSpec Int
              deriving (Eq, Show)

mkMoveSpecParser :: (Int -> MoveSpec) -> Char -> Parser MoveSpec
mkMoveSpecParser mkMove c = mkMove . read <$> (char c >> char ' ' >> many1 digit)

moveSpecParser :: Parser MoveSpec
moveSpecParser = mkMoveSpecParser MoveUpSpec 'U'
              <|> mkMoveSpecParser MoveDownSpec 'D'
              <|> mkMoveSpecParser MoveLeftSpec 'L'
              <|> mkMoveSpecParser MoveRightSpec 'R'

moveSpecsParser :: Parser [MoveSpec]
moveSpecsParser = endBy1 moveSpecParser endOfLine

type X = Int
type Y = Int
type Pos = (X, Y)

mkPos :: X -> Y -> Pos
mkPos x y = (x, y)

isAdjacent :: Pos -> Pos -> Bool
isAdjacent (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

drawPositions :: [Pos] -> String
drawPositions positions =
  let maxWidth = maximum . map fst $ positions
      maxHeight = maximum . map snd $ positions
      minWidth = minimum . map fst $ positions
      minHeight = minimum . map snd $ positions
      positions' = fromList positions
      mkLine y = reverse [if (x, y) `member` positions' then '#' else '.' | x <- [minWidth..maxWidth]]
      field = reverse $ intercalate "\n" [mkLine y | y <- [minHeight..maxHeight]]
  in field

data Move = MoveUp | MoveDown | MoveLeft | MoveRight
          | MoveUpRight | MoveDownRight | MoveDownLeft | MoveUpLeft
          | Stay
          deriving (Eq, Show)

genMoves :: [MoveSpec] -> [Move]
genMoves [] = []
genMoves ((MoveUpSpec n) : others) = replicate n MoveUp <> genMoves others
genMoves ((MoveDownSpec n) : others) = replicate n MoveDown <> genMoves others
genMoves ((MoveLeftSpec n) : others) = replicate n MoveLeft <> genMoves others
genMoves ((MoveRightSpec n) : others) = replicate n MoveRight <> genMoves others

applyMove :: Pos -> Move -> Pos
applyMove (x, y) Stay = (x, y)
applyMove (x, y) MoveUp = (x, y+1)
applyMove (x, y) MoveDown = (x, y-1)
applyMove (x, y) MoveLeft = (x-1, y)
applyMove (x, y) MoveRight = (x+1, y)
applyMove (x, y) MoveUpRight = (x+1, y+1)
applyMove (x, y) MoveDownRight = (x+1, y-1)
applyMove (x, y) MoveDownLeft = (x-1, y-1)
applyMove (x, y) MoveUpLeft = (x-1, y+1)

getTailMove :: Pos -> Pos -> Move
getTailMove headPos@(hx, hy) tailPos@(tx, ty)
  | isAdjacent headPos tailPos = Stay
  | hy == ty = if hx < tx then MoveLeft else MoveRight
  | hx == tx = if hy < ty then MoveDown else MoveUp
  | hx > tx && hy > ty = MoveUpRight
  | hx < tx && hy > ty = MoveUpLeft
  | hx < tx && hy < ty = MoveDownLeft
  | hx > tx && hy < ty = MoveDownRight

applyMoveToHeadAndTail :: Pos -> Pos -> Move -> (Pos, Pos)
applyMoveToHeadAndTail headPos tailPos headMove =
  let headPos' = applyMove headPos headMove
      tailMove = getTailMove headPos' tailPos
      tailPos' = applyMove tailPos tailMove
  in (headPos', tailPos')

updateKnotPositions :: Move -> [Pos] -> [Pos]
updateKnotPositions _ [] = []
updateKnotPositions headMove [headPos] = [applyMove headPos headMove]
updateKnotPositions headMove (headPos : tailPos : otherPos) =
  let headPos' = applyMove headPos headMove
      tailMove = getTailMove headPos' tailPos
  in headPos' : updateKnotPositions tailMove (tailPos : otherPos)

day9 :: IO ()
day9 = do
  -- let input = testInput1
  -- let input = testInput2
  input <- readFile "input/Day9.txt"

  moveSpecs <- case regularParse moveSpecsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  print moveSpecs

  let moves = genMoves moveSpecs
  print moves

  -- part 1
  let headPos = mkPos 0 0
  let tailPos = mkPos 0 0

  let positions = scanl (\(hp, tp) x -> applyMoveToHeadAndTail hp tp x) (headPos, tailPos) moves
  let (headPositions, tailPositions) = unzip positions
  print headPositions
  print tailPositions
  print $ nub tailPositions
  print $ length . nub $ tailPositions

  -- part 2
  let positions = map (\_ -> mkPos 0 0) [0..9]
  let updatedPositions = scanl (flip updateKnotPositions) positions moves
  let tailPositions = (!! 9) . transpose $ updatedPositions

  putStrLn $ drawPositions tailPositions
  -- print $ tailPositions
  -- print $ nub tailPositions
  print $ length . nub $ tailPositions
