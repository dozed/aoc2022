{-# LANGUAGE QuasiQuotes #-}

module Day21 where

import Control.Monad (void)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ
import Util (lstrip, regularParse)

testInput :: String
testInput = lstrip [r|
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
|]

type Id = String
data ExprId = LeafId Id Int
            | AddId Id Id Id
            | SubId Id Id Id
            | MulId Id Id Id
            | DivId Id Id Id
            deriving (Eq, Show)

idParser :: Parser Id
idParser = many1 lower

leafIdParser :: Parser ExprId
leafIdParser = do
  i <- idParser
  void $ string ": "
  n <- int
  return $ LeafId i n

addIdParser :: Parser ExprId
addIdParser = do
  i <- idParser
  void $ string ": "
  x <- idParser
  void $ string " + "
  y <- idParser
  return $ AddId i x y

subIdParser :: Parser ExprId
subIdParser = do
  i <- idParser
  void $ string ": "
  x <- idParser
  void $ string " - "
  y <- idParser
  return $ SubId i x y

mulIdParser :: Parser ExprId
mulIdParser = do
  i <- idParser
  void $ string ": "
  x <- idParser
  void $ string " * "
  y <- idParser
  return $ MulId i x y

divIdParser :: Parser ExprId
divIdParser = do
  i <- idParser
  void $ string ": "
  x <- idParser
  void $ string " / "
  y <- idParser
  return $ DivId i x y

exprIdParser :: Parser ExprId
exprIdParser = try leafIdParser <|> try addIdParser <|> try subIdParser <|> try mulIdParser <|> divIdParser

exprIdsParser :: Parser [ExprId]
exprIdsParser = endBy1 exprIdParser endOfLine

isLeafId :: ExprId -> Bool
isLeafId (LeafId _ _) = True
isLeafId _ = False

getId :: ExprId -> Id
getId (LeafId i _) = i
getId (AddId i _ _) = i
getId (SubId i _ _) = i
getId (MulId i _ _) = i
getId (DivId i _ _) = i

data Expr = Leaf Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Eq, Show)

buildExpr'' :: Map Id ExprId -> Id -> Expr
buildExpr'' exprIds i =
  let e = fromJust . M.lookup i $ exprIds
  in buildExpr' exprIds e

buildExpr' :: Map Id ExprId -> ExprId -> Expr
buildExpr' _ (LeafId _ x) = Leaf x
buildExpr' exprIds (AddId _ ai bi) = Add (buildExpr'' exprIds ai) (buildExpr'' exprIds bi)
buildExpr' exprIds (SubId _ ai bi) = Sub (buildExpr'' exprIds ai) (buildExpr'' exprIds bi)
buildExpr' exprIds (MulId _ ai bi) = Mul (buildExpr'' exprIds ai) (buildExpr'' exprIds bi)
buildExpr' exprIds (DivId _ ai bi) = Div (buildExpr'' exprIds ai) (buildExpr'' exprIds bi)

buildExpr :: [ExprId] -> Expr
buildExpr exprIds =
  let exprIds' = M.fromList $ map (\e -> (getId e, e)) exprIds
      root = fromJust . M.lookup "root" $ exprIds'
      expr = buildExpr' exprIds' root
  in expr

mkIndent :: Int -> String
mkIndent indent = concat . replicate indent $ "  "

prettyShow' :: Int -> Expr -> [String]
prettyShow' indent (Leaf x) = [mkIndent indent <> "Leaf " <> show x]
prettyShow' indent (Add a b) = [mkIndent indent <> "Add"] ++ prettyShow' (indent+1) a ++ prettyShow' (indent+1) b
prettyShow' indent (Sub a b) = [mkIndent indent <> "Sub"] ++ prettyShow' (indent+1) a ++ prettyShow' (indent+1) b
prettyShow' indent (Mul a b) = [mkIndent indent <> "Mul"] ++ prettyShow' (indent+1) a ++ prettyShow' (indent+1) b
prettyShow' indent (Div a b) = [mkIndent indent <> "Div"] ++ prettyShow' (indent+1) a ++ prettyShow' (indent+1) b

prettyShow :: Expr -> String
prettyShow expr = unlines . prettyShow' 0 $ expr

evaluate :: Expr -> Int
evaluate (Leaf x) = x
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Sub a b) = evaluate a - evaluate b
evaluate (Mul a b) = evaluate a * evaluate b
evaluate (Div a b) = evaluate a `div` evaluate b

day21 :: IO ()
day21 = do
  -- let input = testInput
  input <- readFile "input/Day21.txt"

  exprIds <- case regularParse exprIdsParser input of
    Left e -> fail $ show e
    Right xs -> return xs

  print exprIds

  let expr = buildExpr exprIds

  print expr
  putStrLn $ prettyShow expr
  print $ evaluate expr

  return ()