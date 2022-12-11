{-# LANGUAGE QuasiQuotes #-}

module Day7 where

import Control.Monad (forM_, void)
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String
import Text.RawString.QQ

import Util (regularParse)

testInput1 :: String
testInput1 = [r|$ cd /
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

data LogItem = Chdir String
             | ChdirUp
             | Ls
             | Dir String
             | File String Int
             deriving (Eq, Show)

isChdirOrChdirUp :: LogItem -> Bool
isChdirOrChdirUp (Chdir _) = True
isChdirOrChdirUp ChdirUp = True
isChdirOrChdirUp _ = False

chdirUpParser :: Parser LogItem
chdirUpParser = ChdirUp <$ try (string "$ cd ..")

chdirParser :: Parser LogItem
chdirParser = do
  void $ try (string "$ cd ")
  str <- many1 $ oneOf $ ['a'..'z'] ++ ['/']
  return $ Chdir str

lsParser :: Parser LogItem
lsParser = Ls <$ try (string "$ ls")

dirParser :: Parser LogItem
dirParser = do
  void $ try (string "dir ")
  str <- many1 $ oneOf ['a'..'z']
  return $ Dir str

fileParser :: Parser LogItem
fileParser = do
  size <- read <$> many1 digit
  void $ char ' '
  name <- many1 $ oneOf $ ['a'..'z'] ++ ['.']
  return $ File name size

logItemParser :: Parser LogItem
logItemParser = chdirUpParser <|> chdirParser <|> lsParser <|> dirParser <|> fileParser

logItemsParser :: Parser [LogItem]
logItemsParser = endBy1 logItemParser endOfLine

data FileItem = FileItem String Int
  deriving Show

data Directory = Directory {
  dirName :: String,
  dirDirs :: [Directory],
  dirFiles :: [FileItem]
} deriving Show

prettyPrintDirectory :: Directory -> String -> IO ()
prettyPrintDirectory (Directory name dirs files) pad = do
  putStrLn $ pad <> "Directory " <> name <> " [" <> (intercalate ", " . map show $ files) <> "]"
  forM_ dirs $ \d -> do
    prettyPrintDirectory d (pad <> "  ")

handleDirectory :: Directory -> [LogItem] -> (Directory, [LogItem])
handleDirectory dir [] = (dir, [])
handleDirectory dir (ChdirUp : others) = (dir, others)
handleDirectory dir (Ls : others) = handleDirectory dir others
handleDirectory dir ((Dir _) : others) = handleDirectory dir others
handleDirectory dir ((File fileName size) : others) =
  let fileItem = FileItem fileName size
      dir' = dir { dirFiles = dirFiles dir <> [fileItem] }
  in handleDirectory dir' others
handleDirectory dir ((Chdir dirName) : others) =
  let (innerDir, others') = handleDirectory (Directory dirName [] []) others
      dir' = dir { dirDirs = dirDirs dir <> [innerDir] }
  in handleDirectory dir' others'

handleRootLogItem :: [LogItem] -> Directory
handleRootLogItem ((Chdir "/") : xs) =
  case handleDirectory (Directory "/" [] []) xs of
    (dir, []) -> dir
    _ -> undefined
handleRootLogItem _ = undefined

day7 :: IO ()
day7 = do
  let input = testInput1

  print $ regularParse logItemParser "$ cd .."
  print $ regularParse logItemParser "$ cd abc"
  print $ regularParse logItemParser "$ ls"
  print $ regularParse logItemParser "dir d"
  print $ regularParse logItemParser "8033020 d.log"

  print $ regularParse logItemsParser input

  let xs = [Chdir "/",Ls,Dir "a",File "b.txt" 14848514,File "c.dat" 8504156,Dir "d",Chdir "a",Ls,Dir "e",File "f" 29116,File "g" 2557,File "h.lst" 62596,Chdir "e",Ls,File "i" 584,ChdirUp,ChdirUp,Chdir "d",Ls,File "j" 4060174,File "d.log" 8033020,File "d.ext" 5626152,File "k" 7214296]
  let res = handleRootLogItem xs
  print res
  prettyPrintDirectory res ""

