import Test.Hspec

import Data.List (transpose)

import Day9
import Util (regularParse)

day9Spec :: Spec
day9Spec = do
  describe "parseMoveSpecs" $ do
    it "should parse MoveSpecs" $ do
      let txt = "R 4\nU 4\nL 3\n"
      regularParse parseMoveSpecs txt `shouldBe` Right [MoveRightSpec 4, MoveUpSpec 4, MoveLeftSpec 3]

  describe "isAdjacent" $ do
      it "should compute isAdjacent" $ do
        isAdjacent (0, 0) (0, 0) `shouldBe` True
        isAdjacent (0, 1) (0, 0) `shouldBe` True
        isAdjacent (1, 0) (0, 0) `shouldBe` True
        isAdjacent (1, 1) (0, 0) `shouldBe` True
        isAdjacent (2, 1) (0, 0) `shouldBe` False

  describe "genMoves" $ do
    it "should compute Moves from MoveSpecs" $ do
      genMoves [MoveUpSpec 2, MoveRightSpec 3] `shouldBe`
        [MoveUp, MoveUp, MoveRight, MoveRight, MoveRight]

  describe "mkMoveTail" $ do
    it "should compute tail moves" $ do
      getMoveForTail (0, 0) (0, 0) `shouldBe` Stay
      getMoveForTail (2, 0) (0, 0) `shouldBe` MoveRight
      getMoveForTail (0, 2) (0, 0) `shouldBe` MoveUp
      getMoveForTail (-2, 0) (0, 0) `shouldBe` MoveLeft
      getMoveForTail (0, -2) (0, 0) `shouldBe` MoveDown
      getMoveForTail (2, 1) (0, 0) `shouldBe` MoveUpRight
      getMoveForTail (1, 2) (0, 0) `shouldBe` MoveUpRight
      getMoveForTail (-1, 2) (0, 0) `shouldBe` MoveUpLeft
      getMoveForTail (-2, 1) (0, 0) `shouldBe` MoveUpLeft
      getMoveForTail (-2, -1) (0, 0) `shouldBe` MoveDownLeft
      getMoveForTail (-1, -2) (0, 0) `shouldBe` MoveDownLeft
      getMoveForTail (1, -2) (0, 0) `shouldBe` MoveDownRight
      getMoveForTail (2, -1) (0, 0) `shouldBe` MoveDownRight

  describe "applyMove" $ do
    it "should compute correct position" $ do
      applyMove (0, 0) MoveDownRight `shouldBe` (1, -1)

  describe "applyMoveToHeadAndTail" $ do
    it "should produce the same result as updateKnotPositions" $ do
      let moves = [MoveUp, MoveUp, MoveRight, MoveRight, MoveRight]
          posHead = mkPos 0 0
          posTail = mkPos 0 0

      let (headPositions, tailPositions) = unzip $ scanl (\(hp, tp) x -> applyMoveToHeadAndTail hp tp x) (posHead, posTail) moves

      let updatedPositions = transpose $ scanl (flip updateKnotPositions) [posHead, posTail] moves
      let headPositions' = updatedPositions !! 0
      let tailPositions' = updatedPositions !! 1

      headPositions `shouldBe` headPositions'
      tailPositions `shouldBe` tailPositions'

main :: IO ()
main = hspec $ do
  day9Spec
