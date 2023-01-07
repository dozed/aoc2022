{-# LANGUAGE NamedFieldPuns #-}

module Day17b (Block(..), Jet(..), day17, jetsParser,
              BlockCoords, FieldCoords, mkBlockCoords, isAtLeftWall, isAtRightWall, shiftBlockCoordsLeft, shiftBlockCoordsRight,
              BlockInfo(..), mkBlockInfo, isBlocked, canMoveDown', applyJet',
              FieldInfo(..), mergeBlockIntoField, drawField', getStartYShift
              ) where

import Data.Bits (bit, testBit, (.|.), (.&.), shiftL, shiftR, complement, zeroBits)
import Data.List (intercalate)
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

getBaseBlocks :: [Block]
getBaseBlocks = [HLine, Plus, L, VLine, Square]

-- bit-based approach
type BlockCoords = [Word8]
type FieldCoords = [Word8]
type YShift = Int

data BlockInfo = BlockInfo {
  blockCoords :: BlockCoords,
  yShift :: YShift,
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

getStartYShift :: FieldInfo -> YShift
getStartYShift FieldInfo { fieldHeight } = fieldHeight + 3

mkBlockCoords :: Block -> BlockCoords
mkBlockCoords HLine = [bit 2 .|. bit 3 .|. bit 4 .|. bit 5]
mkBlockCoords Plus = [bit 3, bit 2 .|. bit 3 .|. bit 4, bit 3]
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
-- takeBlockTurn' fieldInfo jets blockInfo | trace (drawField' (mergeBlockIntoField blockInfo fieldInfo)) False = undefined
takeBlockTurn' _ [] _ = undefined
takeBlockTurn' fieldInfo@FieldInfo { fieldCoords } (jet:jets) blockInfo =
  let blockInfo' = applyJet' fieldCoords jet blockInfo
  in
    if not (canMoveDown' fieldCoords blockInfo') then
      let fieldInfo' = mergeBlockIntoField blockInfo' fieldInfo
      in (fieldInfo', jets)
    else
      let blockInfo'' = blockInfo' { yShift = yShift blockInfo' - 1 }
      in takeBlockTurn' fieldInfo jets blockInfo''

takeBlocksTurn' :: FieldInfo -> [Jet] -> [Block] -> Int -> FieldInfo
takeBlocksTurn' field jets blocks blocksLeft | trace (show (1000000000000 - blocksLeft)) False = undefined
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

  let blocks = cycle getBaseBlocks
      jets = cycle baseJets

  -- part 1
  let field1 = FieldInfo { fieldCoords = [complement zeroBits], fieldHeight = 1 }
  let field1' = takeBlocksTurn' field1 jets blocks 2022

  putStrLn "Field:"
  putStrLn $ drawField' field1'

  putStrLn $ "Height: " <> show (fieldHeight field1' - 1)

  -- part 2
  let field2 = FieldInfo { fieldCoords = [complement zeroBits], fieldHeight = 1 }
  let field2' = takeBlocksTurn' field2 jets blocks 1000000000000

  putStrLn "Field:"
  putStrLn $ drawField' field2'

  putStrLn $ "Height: " <> show (fieldHeight field2' - 1)
