import Test.Hspec

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
      getMoveForTail (0, 0) (0, 0) `shouldBe` Nothing
      getMoveForTail (2, 0) (0, 0) `shouldBe` Just MoveRight
      getMoveForTail (0, 2) (0, 0) `shouldBe` Just MoveUp
      getMoveForTail (-2, 0) (0, 0) `shouldBe` Just MoveLeft
      getMoveForTail (0, -2) (0, 0) `shouldBe` Just MoveDown
      getMoveForTail (2, 1) (0, 0) `shouldBe` Just MoveUpRight
      getMoveForTail (1, 2) (0, 0) `shouldBe` Just MoveUpRight
      getMoveForTail (-1, 2) (0, 0) `shouldBe` Just MoveUpLeft
      getMoveForTail (-2, 1) (0, 0) `shouldBe` Just MoveUpLeft
      getMoveForTail (-2, -1) (0, 0) `shouldBe` Just MoveDownLeft
      getMoveForTail (-1, -2) (0, 0) `shouldBe` Just MoveDownLeft
      getMoveForTail (1, -2) (0, 0) `shouldBe` Just MoveDownRight
      getMoveForTail (2, -1) (0, 0) `shouldBe` Just MoveDownRight

  describe "applyMove" $ do
    it "should compute correct position" $ do
      applyMove (0, 0) MoveDownRight `shouldBe` (1, -1)

main :: IO ()
main = hspec $ do
  day9Spec
