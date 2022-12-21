{-# LANGUAGE QuasiQuotes #-}

module Day14 where

import Control.Monad (void)
import Data.Function (on)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (lstrip, regularParse, windows)

testInput1 :: String
testInput1 = lstrip [r|
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
|]

type X = Int
type Y = Int
type Pos = (X, Y)
type Path = [Pos]
type PathSegment = (Pos, Pos)

data Elem = Rock Pos | Sand Pos
            deriving (Eq, Show, Ord)

type Field = Set Elem

getPos :: Elem -> Pos
getPos (Rock pos) = pos
getPos (Sand pos) = pos

isSand :: Elem -> Bool
isSand (Sand _) = True
isSand _ = False

getFieldMaxY :: Field -> Int
getFieldMaxY field = snd . maximumBy (compare `on` snd) . map getPos $ S.toList field

showFieldAndSandField :: Field -> String
showFieldAndSandField field =
  let posList = map getPos . S.toList $ field
      minX = fst . minimumBy (compare `on` fst) $ posList
      maxX = fst . maximumBy (compare `on` fst) $ posList
      minY = snd . minimumBy (compare `on` snd) $ posList
      maxY = snd . maximumBy (compare `on` snd) $ posList
      getPixel x y
        | S.member (Rock (x, y)) field = '#'
        | S.member (Sand (x, y)) field = 'o'
        | otherwise = '.'
      xxs = [[getPixel x y | x <- [minX..maxX]] | y <- [minY..maxY]]
      txt = intercalate "\n" xxs
  in txt

sandSource :: Pos
sandSource = (500, 0)

data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)

mkPathSegment :: Pos -> Pos -> PathSegment
mkPathSegment from to = (from, to)

getOrientation :: PathSegment -> Orientation
getOrientation ((x1, y1), (x2, y2))
  | x1 == x2 = Vertical
  | y1 == y2 = Horizontal
  | otherwise = undefined

getRange :: Int -> Int -> [Int]
getRange from to
  | from <= to = [from,(from+1)..to]
  | otherwise = [from,(from-1)..to]

expandPathSegment :: PathSegment -> Set Pos
expandPathSegment seg@((x1, y1), (x2, y2)) =
  case getOrientation seg of
    Horizontal -> S.fromList [(x, y1) | x <- getRange x1 x2]
    Vertical -> S.fromList [(x2, y) | y <- getRange y1 y2]

expandPath :: Path -> Set Pos
expandPath path =
  let pathSegments = map (\xs -> (head xs, head . tail $ xs)) . windows 2 $ path
      field = foldl (\acc seg -> S.union acc (expandPathSegment seg)) S.empty pathSegments
  in field

expandPaths :: [Path] -> Set Pos
expandPaths paths = foldl (\acc path -> S.union acc (expandPath path)) S.empty paths

posParser :: Parser Pos
posParser = do
  x <- read <$> many1 digit
  void $ char ','
  y <- read <$> many1 digit
  return (x, y)

pathParser :: Parser Path
pathParser = sepBy1 posParser (try (string " -> "))

pathsParser :: Parser [Path]
pathsParser = endBy1 pathParser endOfLine

getDownPos :: Pos -> Pos
getDownPos (x, y) = (x, y + 1)

getDownLeftPos :: Pos -> Pos
getDownLeftPos (x, y) = (x - 1, y + 1)

getDownRightPos :: Pos -> Pos
getDownRightPos (x, y) = (x + 1, y + 1)

isBlockedPos :: Field -> Pos -> Bool
isBlockedPos field pos = S.member (Rock pos) field || S.member (Sand pos) field

isFreePos :: Field -> Pos -> Bool
isFreePos field = not . isBlockedPos field

isComeToRest :: Field -> Pos -> Bool
isComeToRest field pos =
  let downPos = getDownPos pos
      downLeftPos = getDownLeftPos pos
      downRightPos = getDownRightPos pos
      isBlockedDown = isBlockedPos field downPos
      isBlockedLeft = isBlockedPos field downLeftPos
      isBlockedRight = isBlockedPos field downRightPos
      isBlocked = isBlockedDown && isBlockedLeft && isBlockedRight
  in isBlocked

getNextPos :: Field -> Pos -> Pos
getNextPos field pos =
  let downPos = getDownPos pos
      downLeftPos = getDownLeftPos pos
      downRightPos = getDownRightPos pos
  in
    if isFreePos field downPos then downPos
    else if isFreePos field downLeftPos then downLeftPos
    else if isFreePos field downRightPos then downRightPos
    else pos

sandFallsIntoEndlessVoid :: Int -> Pos -> Bool
sandFallsIntoEndlessVoid maxY (_, y) = y >= maxY

fallSandUnit :: (Field -> Pos -> (Bool, Maybe Pos)) -> Field -> Pos -> Maybe Pos
fallSandUnit checkStop field sandPos =
  let (stop, sandPosToStore) = checkStop field sandPos
  in if stop then sandPosToStore
     else
       let nextSandPos = getNextPos field sandPos
       in fallSandUnit checkStop field nextSandPos

fallSandUnits :: (Field -> Pos -> (Bool, Maybe Pos)) -> Field -> Field
fallSandUnits checkStop field =
  let startPos = sandSource
      sandPosToStore = Sand <$> fallSandUnit checkStop field startPos
      field' = maybe field (`S.insert` field) sandPosToStore
  in
    if field == field' then field
    else fallSandUnits checkStop field'

checkStopPart1 :: Int -> Field -> Pos -> (Bool, Maybe Pos)
checkStopPart1 maxY field sandPos
    | isComeToRest field sandPos = (True, Just sandPos)
    | sandFallsIntoEndlessVoid maxY sandPos = (True, Nothing)
    | otherwise = (False, Nothing)

checkStopPart2 :: Int -> Field -> Pos -> (Bool, Maybe Pos)
checkStopPart2 maxY field sandPos@(_, y)
    | isComeToRest field sandPos = (True, Just sandPos)
    | y == (maxY + 1) = (True, Just sandPos)
    | otherwise = (False, Nothing)

day14 :: IO ()
day14 = do
  -- let input = testInput1
  input <- readFile "input/Day14.txt"

  paths <- case regularParse pathsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let field = S.map Rock $ expandPaths paths
      maxY = getFieldMaxY field
  putStrLn $ showFieldAndSandField field

  -- part 1
  putStrLn "part 1"
  let field' = fallSandUnits (checkStopPart1 maxY) field
      numSandUnits = S.size . S.filter isSand $ field'
  putStrLn $ showFieldAndSandField field'
  print numSandUnits

  -- part 2
  putStrLn "part 2"
  let field'' = fallSandUnits (checkStopPart2 maxY) field
      numSandUnits' = S.size . S.filter isSand $ field''

  putStrLn $ showFieldAndSandField field''
  print numSandUnits'
