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
          blockInfo'' = blockInfo' { coords = shiftBlockCoordsLeft (coords blockInfo') }
          field' = [complement zeroBits, bit 0 .|. bit 1] :: [Word8]
      isBlocked blockInfo'' field' `shouldBe` True

    it "should detect a non-blocked position" $ do
      let blockInfo = (mkBlockInfo Square) { yShift = 1 }
          field = [complement zeroBits, bit 0 .|. bit 1] :: [Word8]
      isBlocked blockInfo field `shouldBe` False

      let blockInfo' = (mkBlockInfo Square) { yShift = 1 }
          blockInfo'' = blockInfo' { coords = shiftBlockCoordsRight (coords blockInfo') }
          field' = [complement zeroBits, bit 0 .|. bit 1 .|. bit 2] :: [Word8]
      isBlocked blockInfo'' field' `shouldBe` False
