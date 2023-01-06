{-# LANGUAGE NamedFieldPuns #-}

module Day17 (Block(..), Jet(..), day17, jetsParser,
              BlockCoords, FieldCoords, mkBlockCoords, isAtLeftWall, isAtRightWall, shiftBlockCoordsLeft, shiftBlockCoordsRight,
              BlockInfo(..), mkBlockInfo, isBlocked, canMoveDown', applyJet',
              FieldInfo(..), mergeBlockIntoField, drawField'
              ) where

import Data.Bits (bit, testBit, (.|.), (.&.), shiftL, shiftR, complement, zeroBits)
import Data.Function (on)
import Data.List (intercalate, maximumBy)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word8)
import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.String

import Util (regularParse)

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

type X = Int
type Y = Int
type Pos = (X, Y)
type Field = Set Pos

mkField :: Field
mkField = S.fromList [(x,0) | x <- [0..8]]

getBaseBlocks :: [Block]
getBaseBlocks = [HLine, Plus, L, VLine, Square]

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

getMaxY :: Field -> Y
getMaxY = snd . maximumBy (compare `on` snd)

drawField :: Field -> String
drawField field =
  let maxY = getMaxY field
      getPixel x y
        | y == 0 = if x == 0 || x == 8 then '+' else '-'
        | x == 0 || x == 8 = '|'
        | S.member (x, y) field = '#'
        | otherwise = '.'
  in intercalate "\n" [[getPixel x y | x <- [0..8]] | y <- [maxY,maxY-1..0]]

getHeight :: Field -> Int
getHeight = getMaxY

getStartPos :: Field -> Pos
getStartPos field = (3, getMaxY field + 4)

canMove :: (Pos -> Pos) -> Field -> Block -> Pos -> Bool
canMove adjust field block pos =
  let blocks = materialize block (adjust pos)
      isNotBlockedByFieldRock = S.disjoint field blocks
      isBlockedByWalls = any (\(x, y) -> x < 1 || x > 7 || y < 1) blocks
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

-- bit-based approach
type BlockCoords = [Word8]
type FieldCoords = [Word8]
type YShift = Int

data BlockInfo = BlockInfo {
  blockCoords :: BlockCoords,
  yShift :: Int,
  blockHeight :: Int
}

data FieldInfo = FieldInfo {
  fieldCoords :: FieldCoords,
  fieldHeight :: Int
}

drawField' :: FieldInfo -> String
drawField' FieldInfo { fieldCoords, fieldHeight } =
  let getPixel x y
        | y == 0 = if x == 0 || x == 8 then '+' else '-'
        | x == 0 || x == 8 = '|'
        | testBit (fieldCoords !! (y-1)) (x-1) = '#'
        | otherwise = '.'
  in intercalate "\n" [[getPixel x y | x <- [0..8]] | y <- [fieldHeight,fieldHeight-1..0]]

mkBlockCoords :: Block -> BlockCoords
mkBlockCoords HLine = [bit 2 .|. bit 3 .|. bit 4 .|. bit 5]
mkBlockCoords Plus = [bit 2, bit 1 .|. bit 2 .|. bit 3, bit 2]
mkBlockCoords L = [bit 2 .|. bit 3 .|. bit 4, bit 4, bit 4]
mkBlockCoords VLine = [bit 2, bit 2, bit 2, bit 2]
mkBlockCoords Square = [bit 2 .|. bit 3, bit 2 .|. bit 3]

mkBlockInfo :: Block -> BlockInfo
mkBlockInfo HLine = BlockInfo { blockCoords = mkBlockCoords HLine, blockHeight = 1, yShift = 0 }
mkBlockInfo Plus = BlockInfo { blockCoords = mkBlockCoords Plus, blockHeight = 3, yShift = 0 }
mkBlockInfo L = BlockInfo { blockCoords = mkBlockCoords L, blockHeight = 3, yShift = 0 }
mkBlockInfo VLine = BlockInfo { blockCoords = mkBlockCoords VLine, blockHeight = 4, yShift = 0 }
mkBlockInfo Square = BlockInfo { blockCoords = mkBlockCoords Square, blockHeight = 2, yShift = 0 }

isAtLeftWall :: BlockCoords -> Bool
isAtLeftWall = any (`testBit` 0)

isAtRightWall :: BlockCoords -> Bool
isAtRightWall = any (`testBit` 6)

shiftBlockCoordsLeft :: BlockCoords -> BlockCoords
shiftBlockCoordsLeft coords
  | isAtLeftWall coords = coords
  | otherwise = map (`shiftR` 1) coords

shiftBlockCoordsRight :: BlockCoords -> BlockCoords
shiftBlockCoordsRight coords
  | isAtRightWall coords = coords
  | otherwise = map (`shiftL` 1) coords

isBlocked :: BlockInfo -> FieldCoords -> Bool
isBlocked BlockInfo { blockCoords, yShift } fieldCoords =
  let relevantFieldCoords = drop yShift fieldCoords
      toCompare = blockCoords `zip` relevantFieldCoords
  in any (\(bs, fs) -> bs .&. fs > 0) toCompare

applyJet' :: FieldCoords -> Jet -> BlockInfo -> BlockInfo
applyJet' fieldCoords JetLeft blockInfo@BlockInfo { blockCoords } =
  let blockCoords' = shiftBlockCoordsLeft blockCoords
      blockInfo' = blockInfo { blockCoords = blockCoords' }
      blockInfo'' = if isBlocked blockInfo' fieldCoords then blockInfo else blockInfo'
  in blockInfo''
applyJet' fieldCoords JetRight blockInfo@BlockInfo { blockCoords } =
  let blockCoords' = shiftBlockCoordsRight blockCoords
      blockInfo' = blockInfo { blockCoords = blockCoords' }
      blockInfo'' = if isBlocked blockInfo' fieldCoords then blockInfo else blockInfo'
  in blockInfo''

canMoveDown' :: FieldCoords -> BlockInfo -> Bool
canMoveDown' fieldCoords blockInfo =
  let blockInfo' = blockInfo { yShift = yShift blockInfo - 1 }
  in not (isBlocked blockInfo' fieldCoords)

mergeBlockIntoField :: BlockInfo -> FieldInfo -> FieldInfo
--mergeBlockIntoField BlockInfo { blockCoords, yShift, blockHeight } fieldInfo@FieldInfo { fieldCoords, fieldHeight }
--  | trace ("yShift: " <> show yShift <> " blockHeight: " <> show blockHeight <> " fieldHeight: " <> show fieldHeight) False = undefined
mergeBlockIntoField BlockInfo { blockCoords, yShift, blockHeight } fieldInfo@FieldInfo { fieldCoords, fieldHeight } =
  let rowsToAdd = max 0 ((yShift + blockHeight) - fieldHeight)
      fieldHeight' = fieldHeight + rowsToAdd
      fieldCoords' = fieldCoords ++ replicate rowsToAdd zeroBits
      modRow row i = if yShift <= i && i < yShift + blockHeight then row .|. (blockCoords !! (i - yShift)) else row
      fieldCoords'' = reverse $ foldl (\acc (row, i) -> modRow row i:acc) [] (fieldCoords' `zip` [0..])
      fieldInfo' = fieldInfo { fieldCoords = fieldCoords'', fieldHeight = fieldHeight' }
  in fieldInfo'

takeBlockTurn' :: FieldInfo -> [Jet] -> BlockInfo -> (FieldInfo, [Jet])
-- takeBlockTurn' fieldCoords jets block | trace (drawField (S.union field (materialize block pos))) False = undefined
takeBlockTurn' _ [] _ = undefined
takeBlockTurn' fieldInfo@FieldInfo { fieldCoords } (jet:jets) blockInfo =
  let blockInfo' = applyJet' fieldCoords jet blockInfo
  in
    if not (canMoveDown' fieldCoords blockInfo') then
      let fieldInfo' = mergeBlockIntoField blockInfo' fieldInfo
      in (fieldInfo', jets)
    else
      let blockInfo'' = blockInfo { yShift = yShift blockInfo - 1 }
      in takeBlockTurn' fieldInfo jets blockInfo''

getStartYShift :: FieldInfo -> Int
getStartYShift FieldInfo { fieldHeight } = fieldHeight + 3

takeBlocksTurn' :: FieldInfo -> [Jet] -> [Block] -> Int -> FieldInfo
-- takeBlocksTurn' field jets blocks blocksLeft | trace (show (1000000000000 - blocksLeft)) False = undefined
takeBlocksTurn' fieldInfo _ _ 0 = fieldInfo
takeBlocksTurn' _ _ [] _ = undefined
takeBlocksTurn' fieldInfo jets (block:blocks) blocksLeft =
  let startYShift = getStartYShift fieldInfo
      blockInfo = (mkBlockInfo block) { yShift = startYShift }
      (fieldInfo', jets') = takeBlockTurn' fieldInfo jets blockInfo
  in takeBlocksTurn' fieldInfo' jets' blocks (blocksLeft-1)

day17 :: IO ()
day17 = do
  -- let input = testInput
  input <- readFile "input/Day17.txt"

  baseJets <- case regularParse jetsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  let field = mkField
      blocks = cycle getBaseBlocks
      jets = cycle baseJets

  -- part 1
  let field' = takeBlocksTurn field jets blocks 2022
      height = getHeight field'

  putStrLn "Field:"
  putStrLn $ drawField field'

  putStrLn $ "Height: " <> show height

  -- part 2
  --  let cycleSize = length getBaseBlocks * length baseJets
  --  putStrLn $ "cycleSize: " <> show cycleSize
  --  print $ take 100 (blocks `zip` jets)
  --  print $ take 100 $ drop cycleSize (blocks `zip` jets)

  let fieldInfo = FieldInfo { fieldCoords = [complement zeroBits], fieldHeight = 1 }
  let field'' = takeBlocksTurn' fieldInfo jets blocks 2022

  putStrLn $ "Field height: " <> show (fieldHeight field'')

