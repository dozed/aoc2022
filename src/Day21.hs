{-# LANGUAGE QuasiQuotes #-}

module Day21 where

import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
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

data Expr = Leaf Id Int
          | Add Id Expr Expr
          | Sub Id Expr Expr
          | Mul Id Expr Expr
          | Div Id Expr Expr
          deriving (Eq, Show)

buildExpr'' :: Map Id ExprId -> Id -> Expr
buildExpr'' exprIds i =
  let e = fromJust . M.lookup i $ exprIds
  in buildExpr' exprIds e

buildExpr' :: Map Id ExprId -> ExprId -> Expr
buildExpr' _ (LeafId i x) = Leaf i x
buildExpr' exprIds (AddId i ai bi) = Add i (buildExpr'' exprIds ai) (buildExpr'' exprIds bi)
buildExpr' exprIds (SubId i ai bi) = Sub i (buildExpr'' exprIds ai) (buildExpr'' exprIds bi)
buildExpr' exprIds (MulId i ai bi) = Mul i (buildExpr'' exprIds ai) (buildExpr'' exprIds bi)
buildExpr' exprIds (DivId i ai bi) = Div i (buildExpr'' exprIds ai) (buildExpr'' exprIds bi)

buildExpr :: [ExprId] -> Expr
buildExpr exprIds =
  let exprIds' = M.fromList $ map (\e -> (getId e, e)) exprIds
      root = fromJust . M.lookup "root" $ exprIds'
      expr = buildExpr' exprIds' root
  in expr

mkIndent :: Int -> String
mkIndent indent = concat . replicate indent $ "  "

prettyShow' :: Int -> Expr -> [String]
prettyShow' indent (Leaf i x) = [mkIndent indent <> "Leaf " <> show i <> " " <> show x]
prettyShow' indent (Add i a b) = [mkIndent indent <> "Add " <> show i] ++ prettyShow' (indent+1) a ++ prettyShow' (indent+1) b
prettyShow' indent (Sub i a b) = [mkIndent indent <> "Sub " <> show i] ++ prettyShow' (indent+1) a ++ prettyShow' (indent+1) b
prettyShow' indent (Mul i a b) = [mkIndent indent <> "Mul " <> show i] ++ prettyShow' (indent+1) a ++ prettyShow' (indent+1) b
prettyShow' indent (Div i a b) = [mkIndent indent <> "Div " <> show i] ++ prettyShow' (indent+1) a ++ prettyShow' (indent+1) b

prettyShow :: Expr -> String
prettyShow expr = unlines . prettyShow' 0 $ expr

evaluate :: Expr -> Int
evaluate (Leaf _ x) = x
evaluate (Add _ a b) = evaluate a + evaluate b
evaluate (Sub _ a b) = evaluate a - evaluate b
evaluate (Mul _ a b) = evaluate a * evaluate b
evaluate (Div _ a b) = evaluate a `div` evaluate b

getSides :: Expr -> (Expr, Expr)
getSides (Leaf _ _) = error "getSides on Leaf"
getSides (Add _ a b) = (a, b)
getSides (Sub _ a b) = (a, b)
getSides (Mul _ a b) = (a, b)
getSides (Div _ a b) = (a, b)

data Side = L | R
            deriving (Eq, Show)

containsHumanInput :: Expr -> Bool
containsHumanInput (Leaf "humn" _) = True
containsHumanInput (Leaf _ _) = False
containsHumanInput (Add _ a b) = containsHumanInput a || containsHumanInput b
containsHumanInput (Sub _ a b) = containsHumanInput a || containsHumanInput b
containsHumanInput (Mul _ a b) = containsHumanInput a || containsHumanInput b
containsHumanInput (Div _ a b) = containsHumanInput a || containsHumanInput b

moveFromLhsToRhs :: Expr -> Expr -> (Expr, Expr)
moveFromLhsToRhs (Leaf _ _) _ = error "moveFromLhsToRhs on Leaf"
moveFromLhsToRhs (Add _ a b) rhs = if containsHumanInput a then (a, Sub "" rhs b) else (b, Sub "" rhs a)
moveFromLhsToRhs (Sub _ a b) rhs = if containsHumanInput a then (a, Add "" rhs b) else (b, Mul "" (Sub "" rhs a) (Leaf "" (-1)))
moveFromLhsToRhs (Mul _ a b) rhs = if containsHumanInput a then (a, Div "" rhs b) else (b, Div "" rhs a)
moveFromLhsToRhs (Div _ a b) rhs = if containsHumanInput a then (a, Mul "" rhs b) else (b, Mul "" a (Div "" (Leaf "" 1) rhs))

isReduced :: Expr -> Bool
isReduced (Leaf "humn" _) = True
isReduced _ = False

reduce :: Expr -> Expr -> Expr
reduce lhs rhs | isReduced lhs = rhs
reduce lhs rhs =
  let (lhs', rhs') = moveFromLhsToRhs lhs rhs
  in reduce lhs' rhs'

day21 :: IO ()
day21 = do
  -- let input = testInput
  input <- readFile "input/Day21.txt"

  exprIds <- case regularParse exprIdsParser input of
    Left e -> fail $ show e
    Right xs -> return xs

  print exprIds

  -- part 1
  let expr = buildExpr exprIds
  print expr
  putStrLn $ prettyShow expr
  print $ evaluate expr

  -- part 2
  let (lhs, rhs) = getSides expr
      (lhs', rhs') = if containsHumanInput lhs then (lhs, rhs) else (rhs, lhs)
      rhs'' = reduce lhs' rhs'

  print $ evaluate rhs''
