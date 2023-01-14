module Day17 (Block(..), Jet(..), day17, jetsParser, showField, readField,
              X, Y, Pos, Field, isAdjacent, getMaxY, getDownPos, getUpPos, getLeftPos, getRightPos, getStartPos,
              canMoveDown, canMoveLeft, canMoveRight, getFreePosInRow, getRockPosInRow,
              isBlockedByWall, isBlockedByFieldRock, getReachables, traceWaveFront
              ) where

import Data.Function (on)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.String

import Util (filterNot, regularParse)

testInput :: String
testInput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

data Jet = JetLeft | JetRight
           deriving (Eq, Show)

jetsParser :: Parser [Jet]
jetsParser = many1 $ (JetLeft <$ char '<') <|> (JetRight <$ char '>')

data Block = HLine
           | Plus
           | L
           | VLine
           | Square
           deriving (Eq, Show)

getBaseBlocks :: [Block]
getBaseBlocks = [HLine, Plus, L, VLine, Square]

type X = Int
type Y = Int
type Pos = (X, Y)
type Field = Set Pos

isAdjacent :: Pos -> Pos -> Bool
isAdjacent (x1, y1) (x2, y2)
  | x1 == x2 = abs(y1 - y2) == 1
  | y1 == y2 = abs(x1 - x2) == 1
  | otherwise = False

getMaxY :: Field -> Y
getMaxY = snd . maximumBy (compare `on` snd)

showField :: Field -> String
showField field =
  let maxY = getMaxY field
      getPixel x y
        | S.member (x, y) field = '#'
        | otherwise = '.'
  in (intercalate "\n" [['|'] ++ [getPixel x y | x <- [0..6]] ++ ['|'] | y <- [maxY,maxY-1..0]]) ++ "\n+-------+"

readField :: String -> Set Pos
readField fieldTxt =
  let fieldLines = reverse . map (init . tail) . init . lines $ fieldTxt
      toPos c x y = if c == '#' then Just ((x, y) :: Pos) else Nothing
      pos = S.fromList . concatMap (\(row, y) -> mapMaybe (\(c, x) -> toPos c x y) $ row `zip` [0..]) $ fieldLines `zip` [0..]
  in pos

mkField :: Field
mkField = S.fromList [(x,0) | x <- [0..8]]

getUpPos :: Pos -> Pos
getUpPos (x, y) = (x, y + 1)

getDownPos :: Pos -> Pos
getDownPos (x, y) = (x, y - 1)

getLeftPos :: Pos -> Pos
getLeftPos (x, y) = (x - 1, y)

getRightPos :: Pos -> Pos
getRightPos (x, y) = (x + 1, y)

materialize :: Block -> Pos -> Set Pos
materialize HLine (x, y) = S.fromList [(x, y), (x+1, y), (x+2, y), (x+3, y)]
materialize Plus (x, y) = S.fromList [(x+1, y), (x, y+1), (x+1, y+1), (x+2, y+1), (x+1, y+2)]
materialize L (x, y) = S.fromList [(x, y), (x+1, y), (x+2, y), (x+2, y+1), (x+2, y+2)]
materialize VLine (x, y) = S.fromList [(x, y), (x, y+1), (x, y+2), (x, y+3)]
materialize Square (x, y) = S.fromList [(x, y), (x+1, y), (x, y+1), (x+1, y+1)]

getHeight :: Field -> Int
getHeight = getMaxY

getStartPos :: Field -> Pos
getStartPos field = (2, getMaxY field + 4)

isBlockedByWall :: Pos -> Bool
isBlockedByWall (x, y) = x < 0 || x > 6 || y < 0

isBlockedByFieldRock :: Field -> Pos -> Bool
isBlockedByFieldRock field block = S.member block field

isBlocked :: Field -> Pos -> Bool
isBlocked field pos = isBlockedByWall pos || isBlockedByFieldRock field pos

canMove :: (Pos -> Pos) -> Field -> Block -> Pos -> Bool
canMove adjust field block pos =
  let blocks = materialize block (adjust pos)
      isNotBlockedByFieldRock = S.disjoint field blocks
      isBlockedByWalls = any isBlockedByWall blocks
      isNotBlocked = isNotBlockedByFieldRock && not isBlockedByWalls
  in isNotBlocked

canMoveDown :: Field -> Block -> Pos -> Bool
canMoveDown = canMove (\(x, y) -> (x, y - 1))

canMoveLeft :: Field -> Block -> Pos -> Bool
canMoveLeft = canMove (\(x, y) -> (x - 1, y))

canMoveRight :: Field -> Block -> Pos -> Bool
canMoveRight = canMove (\(x, y) -> (x + 1, y))

applyJet :: Field -> Jet -> Block -> Pos -> Pos
applyJet field JetLeft block pos = if canMoveLeft field block pos then getLeftPos pos else pos
applyJet field JetRight block pos = if canMoveRight field block pos then getRightPos pos else pos

takeBlockTurn :: Field -> [Jet] -> Block -> Pos -> (Field, [Jet])
-- takeBlockTurn field jets block pos | trace (drawField (S.union field (materialize block pos))) False = undefined
takeBlockTurn _ [] _ _ = undefined
takeBlockTurn field (jet:jets) block pos =
  let pos' = applyJet field jet block pos
  in
    if not (canMoveDown field block pos') then
      let blocks = materialize block pos'
          field' = S.union field blocks
      in (field', jets)
    else
      let pos'' = getDownPos pos'
      in takeBlockTurn field jets block pos''

takeBlocksTurn :: Field -> [Jet] -> [Block] -> Int -> Field
takeBlocksTurn field jets blocks blocksLeft | trace (show (1000000000000 - blocksLeft)) False = undefined
takeBlocksTurn field _ _ 0 = field
takeBlocksTurn _ _ [] _ = undefined
takeBlocksTurn field jets (block:blocks) blocksLeft =
  let startPos = getStartPos field
      (field', jets') = takeBlockTurn field jets block startPos
  in takeBlocksTurn field' jets' blocks (blocksLeft-1)

getFreePosInRow :: Field -> Y -> [Pos]
getFreePosInRow field y = [(x, y) | x <- [0..6], not (S.member (x, y) field)]

getRockPosInRow :: Field -> Y -> [Pos]
getRockPosInRow field y = [(x, y) | x <- [0..6], S.member (x, y) field]

type Visited = Set Pos
type ToVisit = [Pos]

getReachables' :: Field -> ToVisit -> Visited -> Set Pos
-- getReachables' _ toVisit visited | trace (show toVisit <> " - " <> show visited) False = undefined
getReachables' _ [] visited = visited
getReachables' field (current:nextToVisit) visited =
  let visited' = S.insert current visited
      neighbours = filterNot (isBlocked field) [getLeftPos current, getRightPos current, getDownPos current]
      neighboursNotVisited = filterNot (`S.member` visited') neighbours
      neighboursNotVisitedAndNotToVisit = filterNot (`elem` nextToVisit) neighboursNotVisited
      nextToVisit' = nextToVisit ++ neighboursNotVisitedAndNotToVisit
  in getReachables' field nextToVisit' visited'

getReachables :: Field -> Set Pos
getReachables field =
  let maxY = getMaxY field
      freePos = filterNot (isBlockedByFieldRock field) [(x, maxY) | x <- [0..6]]
      reachables = getReachables' field freePos S.empty
  in reachables

traceWaveFront :: Field -> Set Pos
traceWaveFront field =
  let reachables = getReachables field
      lowY = snd . minimumBy (compare `on` snd) $ reachables
      isAdjacentToReachable pos = any (isAdjacent pos) reachables
      waveFront = S.filter (\rp@(_, y) -> y >= lowY - 1 && isAdjacentToReachable rp) field
  in waveFront

day17 :: IO ()
day17 = do
  -- let input = testInput
  input <- readFile "input/Day17.txt"

  baseJets <- case regularParse jetsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let blocks = cycle getBaseBlocks
      jets = cycle baseJets

  -- part 1
--  let field1 = mkField
--      field1' = takeBlocksTurn field1 jets blocks 2022
--
--  putStrLn "Field:"
--  putStrLn $ drawField field1'
--
--  putStrLn $ "Height: " <> show (getHeight field1')

  -- part 2
  --  let cycleSize = length getBaseBlocks * length baseJets
  --  putStrLn $ "cycleSize: " <> show cycleSize
  --  print $ take 100 (blocks `zip` jets)
  --  print $ take 100 $ drop cycleSize (blocks `zip` jets)

  let field2 = mkField
      field2' = takeBlocksTurn field2 jets blocks 15

  putStrLn "Field:"
  putStrLn $ showField field2'

--  let field3 = takeBlocksTurn field1 jets blocks 1000000000000
--
--  putStrLn "Field:"
--  putStrLn $ drawField field3
--
--  putStrLn $ "Height: " <> show (getHeight field3)
