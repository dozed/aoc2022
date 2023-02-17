module Day21Spec where

import Test.Hspec

import Day21
import Util (regularParse)

day21Spec :: Spec
day21Spec = do

  describe "exprIdsParser" $ do
    it "should parse a list of ExprId" $ do
      let input = testInput

      exprIds <- case regularParse exprIdsParser input of
        Left e -> fail $ show e
        Right xs -> return xs

      let expected = [
              AddId "root" "pppw" "sjmn", LeafId "dbpl" 5, AddId "cczh" "sllz" "lgvd", LeafId "zczc" 2,
              SubId "ptdq" "humn" "dvpt", LeafId "dvpt" 3, LeafId "lfqf" 4, LeafId "humn" 5, LeafId "ljgn" 2,
              MulId "sjmn" "drzm" "dbpl", LeafId "sllz" 4, DivId "pppw" "cczh" "lfqf", MulId "lgvd" "ljgn" "ptdq",
              SubId "drzm" "hmdt" "zczc", LeafId "hmdt" 32
            ]

      exprIds `shouldBe` expected

  describe "buildExpr" $ do
    it "should build an Expr from a list of ExprId" $ do
      let exprIds = [
              AddId "root" "pppw" "sjmn", LeafId "dbpl" 5, AddId "cczh" "sllz" "lgvd", LeafId "zczc" 2,
              SubId "ptdq" "humn" "dvpt", LeafId "dvpt" 3, LeafId "lfqf" 4, LeafId "humn" 5, LeafId "ljgn" 2,
              MulId "sjmn" "drzm" "dbpl", LeafId "sllz" 4, DivId "pppw" "cczh" "lfqf", MulId "lgvd" "ljgn" "ptdq",
              SubId "drzm" "hmdt" "zczc", LeafId "hmdt" 32
            ]
          expected = Add "root" (Div "pppw" (Add "cczh" (Leaf "sllz" 4) (Mul "lgvd" (Leaf "ljgn" 2) (Sub "ptdq" (Leaf "humn" 5) (Leaf "dvpt" 3)))) (Leaf "lfqf" 4)) (Mul "sjmn" (Sub "drzm" (Leaf "hmdt" 32) (Leaf "zczc" 2)) (Leaf "dbpl" 5))

      buildExpr exprIds `shouldBe` expected

  describe "evaluate" $ do
    it "should evaluate an Expr" $ do
      let expr = Add "root" (Div "pppw" (Add "cczh" (Leaf "sllz" 4) (Mul "lgvd" (Leaf "ljgn" 2) (Sub "ptdq" (Leaf "humn" 5) (Leaf "dvpt" 3)))) (Leaf "lfqf" 4)) (Mul "sjmn" (Sub "drzm" (Leaf "hmdt" 32) (Leaf "zczc" 2)) (Leaf "dbpl" 5))

      evaluate expr `shouldBe` 152
