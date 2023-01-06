{-# LANGUAGE ScopedTypeVariables #-}

module Day17Spec (day17Spec) where

import Data.Bits
import Data.Word

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, elements)

import Day17
import Util (regularParse)

instance Arbitrary Block where
  arbitrary = do
    elements [HLine, Plus, L, VLine, Square]

day17Spec :: Spec
day17Spec = do

  describe "jetsParser" $ do
    it "should parse a list of jet descriptions" $ do
      regularParse jetsParser ">><" `shouldBe` Right [JetRight, JetRight, JetLeft]

  describe "isAtLeftWall" $ do
    it "should detect BlockCoords at the left wall" $ do
      let coords = mkBlockCoords Plus
      isAtLeftWall coords `shouldBe` False
      isAtLeftWall (shiftBlockCoordsLeft coords) `shouldBe` True

  describe "isAtRightWall" $ do
    it "should detect BlockCoords at the right wall" $ do
      let coords = mkBlockCoords Plus
      isAtRightWall coords `shouldBe` False
      isAtRightWall (shiftBlockCoordsRight coords) `shouldBe` False
      isAtRightWall (shiftBlockCoordsRight . shiftBlockCoordsRight $ coords) `shouldBe` False
      isAtRightWall (shiftBlockCoordsRight . shiftBlockCoordsRight . shiftBlockCoordsRight $ coords) `shouldBe` True

  describe "shiftBlockCoordsLeft" $ do
    it "should return same coords if at left wall" $ do
      let coords = shiftBlockCoordsLeft $ mkBlockCoords Plus
      shiftBlockCoordsLeft coords `shouldBe` coords

    prop "is inverse of shiftBlockCoordsRight" $ \block -> do
      let coords = mkBlockCoords block
      shiftBlockCoordsRight (shiftBlockCoordsLeft coords) `shouldBe` coords
      shiftBlockCoordsLeft (shiftBlockCoordsRight coords) `shouldBe` coords

  describe "shiftBlockCoordsRights" $ do
    it "should return same coords if at right wall" $ do
      let coords = shiftBlockCoordsRight . shiftBlockCoordsRight . shiftBlockCoordsRight $ mkBlockCoords Plus
      shiftBlockCoordsRight coords `shouldBe` coords

  describe "isBlocked" $ do
    it "should detect a blocked position" $ do
      let blockInfo = (mkBlockInfo Square) { yShift = 1 }
          field = [complement zeroBits, bit 0 .|. bit 1 .|. bit 2] :: [Word8]
      isBlocked blockInfo field `shouldBe` True

      let blockInfo' = (mkBlockInfo Square) { yShift = 1 }
          blockInfo'' = blockInfo' { blockCoords = shiftBlockCoordsLeft (blockCoords blockInfo') }
          field' = [complement zeroBits, bit 0 .|. bit 1] :: [Word8]
      isBlocked blockInfo'' field' `shouldBe` True

    it "should detect a non-blocked position" $ do
      let blockInfo1 = (mkBlockInfo Square) { yShift = 1 }
          field1 = [complement zeroBits, bit 0 .|. bit 1] :: [Word8]
      isBlocked blockInfo1 field1 `shouldBe` False

      let blockInfo2 = (mkBlockInfo Square) { yShift = 1 }
          blockInfo2' = blockInfo2 { blockCoords = shiftBlockCoordsRight (blockCoords blockInfo2) }
          field2 = [complement zeroBits, bit 0 .|. bit 1 .|. bit 2] :: [Word8]
      isBlocked blockInfo2' field2 `shouldBe` False

      let blockInfo3 = (mkBlockInfo Square) { yShift = 2 }
          field3 = [complement zeroBits, bit 0 .|. bit 1 .|. bit 2] :: [Word8]
      isBlocked blockInfo3 field3 `shouldBe` False

  describe "canMoveDown'" $ do
    it "should detect that a block can move down" $ do
      let blockInfo = (mkBlockInfo Square) { yShift = 3 }
          field = [complement zeroBits, bit 0 .|. bit 1 .|. bit 2] :: [Word8]

      canMoveDown' field blockInfo `shouldBe` True

      let blockInfo2 = (mkBlockInfo Square) { yShift = 3 }
          field2 = [complement zeroBits, bit 0 .|. bit 1] :: [Word8]

      canMoveDown' field2 blockInfo2 `shouldBe` True
