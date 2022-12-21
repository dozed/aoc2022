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
type Field = Set Pos

showFieldAndSandField :: Field -> Field -> String
showFieldAndSandField field sandField =
  let fullField = S.union field sandField
      minX = fst . minimumBy (compare `on` fst) $ fullField
      maxX = fst . maximumBy (compare `on` fst) $ fullField
      minY = snd . minimumBy (compare `on` snd) $ fullField
      maxY = snd . maximumBy (compare `on` snd) $ fullField
      getPixel x y
        | S.member (x, y) field = '#'
        | S.member (x, y) sandField = 'o'
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

expandPathSegment :: PathSegment -> Field
expandPathSegment seg@((x1, y1), (x2, y2)) =
  case getOrientation seg of
    Horizontal -> S.fromList [(x, y1) | x <- getRange x1 x2]
    Vertical -> S.fromList [(x2, y) | y <- getRange y1 y2]

expandPath :: Path -> Field
expandPath path =
  let pathSegments = map (\xs -> (head xs, head . tail $ xs)) . windows 2 $ path
      field = foldl (\acc seg -> S.union acc (expandPathSegment seg)) S.empty pathSegments
  in field

expandPaths :: [Path] -> Field
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
isBlockedPos field pos = S.member pos field

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

sandFallsIntoEndlessVoid :: Field -> Pos -> Bool
sandFallsIntoEndlessVoid field (_, y) =
  let maxY = snd . maximumBy (compare `on` snd) $ S.toList field
  in y >= maxY

fallSandUnit :: (Field -> Pos -> (Bool, Maybe Pos)) -> Field -> Field -> Pos -> Maybe Pos
fallSandUnit checkStop field sandField sandPos =
  let unionField = S.union field sandField
      (stop, newPos) = checkStop unionField sandPos
  in if stop then newPos
     else
       let nextSandPos = getNextPos unionField sandPos
       in fallSandUnit checkStop field sandField nextSandPos

fallSandUnits :: (Field -> Pos -> (Bool, Maybe Pos)) -> Field -> Field -> Field
fallSandUnits checkStop field sandField =
  let startPos = sandSource
      newSandPos = fallSandUnit checkStop field sandField startPos
      sandField' = maybe sandField (`S.insert` sandField) newSandPos
  in
    if sandField == sandField' then sandField
    else fallSandUnits checkStop field sandField'

day14 :: IO ()
day14 = do
  -- let input = testInput1
  input <- readFile "input/Day14.txt"

  paths <- case regularParse pathsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let field = expandPaths paths
  putStrLn $ showFieldAndSandField field S.empty

  -- part 1
  putStrLn "part 1"
  let checkStop unionField sandPos
        | isComeToRest unionField sandPos = (True, Just sandPos)
        | sandFallsIntoEndlessVoid unionField sandPos = (True, Nothing)
        | otherwise = (False, Nothing)

  let sandField' = fallSandUnits checkStop field S.empty
  putStrLn $ showFieldAndSandField field sandField'

  let numSandUnits = S.size sandField'
  print numSandUnits

  -- part 2
  putStrLn "part 2"
  let maxY = snd . maximumBy (compare `on` snd) $ S.toList field

  let checkStop' unionField sandPos@(_, y)
        | isComeToRest unionField sandPos = (True, Just sandPos)
        | y == (maxY + 2) = (True, Just sandPos)
        | otherwise = (False, Nothing)

  let sandField'' = fallSandUnits checkStop' field S.empty
  putStrLn $ showFieldAndSandField field sandField''

  let numSandUnits' = S.size sandField''
  print numSandUnits'
