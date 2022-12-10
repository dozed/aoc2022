{-# LANGUAGE QuasiQuotes #-}

module Day7 where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse)

testInput1 :: String
testInput1 = [r|
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|]

chdirUpParser :: Parser Input
chdirUpParser = ChdirUp <$ try (string "$ cd ..")

chdirParser :: Parser Input
chdirParser = do
  void $ try (string "$ cd ")
  str <- many1 $ oneOf ['a'..'z']
  return $ Chdir str

lsParser :: Parser Input
lsParser = Ls <$ try (string "$ ls")

dirParser :: Parser Input
dirParser = do
  void $ try (string "dir ")
  str <- many1 $ oneOf ['a'..'z']
  return $ Dir str  

fileParser :: Parser Input
fileParser = do
  size <- read <$> many1 digit
  void $ char ' '
  name <- many1 $ oneOf $ ['a'..'z'] ++ ['.']
  return $ File size name  

inputParser :: Parser Input
inputParser = chdirUpParser <|> chdirParser <|> lsParser <|> dirParser <|> fileParser

data Input = Chdir String
           | ChdirUp
           | Ls
           | Dir String
           | File Int String
           deriving Show

day7 :: IO ()
day7 = do
  print "day7"

  print $ regularParse inputParser "$ cd .."
  print $ regularParse inputParser "$ cd abc"
  print $ regularParse inputParser "$ ls"
  print $ regularParse inputParser "dir d"
  print $ regularParse inputParser "8033020 d.log"
