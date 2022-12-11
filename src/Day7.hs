{-# LANGUAGE QuasiQuotes #-}

module Day7 where

import Control.Monad (forM_, void)
import Data.Function (on)
import Data.List (intercalate, minimumBy)
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

data FileItem = FileItem {
  fileItemName :: String,
  fileItemSize :: Int
} deriving Show

data Directory = Directory {
  dirName :: String,
  dirDirs :: [Directory],
  dirFiles :: [FileItem]
} deriving Show

flattenDirectory :: Directory -> [Directory]
flattenDirectory dir = dir : (concatMap flattenDirectory . dirDirs $ dir)

directorySize :: Directory -> Int
directorySize dir =
  let currentSize = sum . map fileItemSize . dirFiles $ dir
      recSize = sum . map directorySize . dirDirs $ dir
      totalSize = currentSize + recSize
  in totalSize

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

handleRootLogItem :: [LogItem] -> Either String Directory
handleRootLogItem ((Chdir "/") : xs) =
  case handleDirectory (Directory "/" [] []) xs of
    (dir, []) -> Right dir
    _ -> Left "stream of log items could not be consumed fully"
handleRootLogItem _ = Left "stream of log items does not start with (Chdir \"/\")"

day7 :: IO ()
day7 = do
  -- let input = testInput1
  input <- readFile "input/Day7.txt"

  logItems <- case regularParse logItemsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  rootDir <- case handleRootLogItem logItems of
    Left e -> fail e
    Right dir -> pure dir

  let dirsWithSize = map (\d -> (d, directorySize d)) . flattenDirectory $ rootDir

  -- part 1
  let smallDirs = filter (\(_, s) -> s <= 100000) dirsWithSize

  forM_ smallDirs $ \(d, s) -> do
    print (d, s)

  print $ sum . map snd $ smallDirs

  -- part 2
  let totalDiskSpace = 70000000
      requiredSpace = 30000000
      usedSpace = directorySize rootDir
      unusedSpace = totalDiskSpace - usedSpace
      minToDeleteSpace = requiredSpace - unusedSpace

  putStrLn $ "usedSpace: " <> show usedSpace
  putStrLn $ "unusedSpace: " <> show unusedSpace
  putStrLn $ "minToDeleteSpace: " <> show minToDeleteSpace

  let largeDirs = filter (\(_, s) -> s >= minToDeleteSpace) dirsWithSize
      (toDeleteDir, toDeleteDirSize) = minimumBy (compare `on` snd) largeDirs

  print toDeleteDir
  print toDeleteDirSize

