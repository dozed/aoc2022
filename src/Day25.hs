{-# LANGUAGE QuasiQuotes #-}

module Day25 where

import Text.RawString.QQ

import Util (lstrip)

testInput :: String
testInput = lstrip [r|
1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
|]

getBase5 :: Integer -> [Integer]
getBase5 x =
  let (d, m) = x `divMod` 5
  in if d == 0 then [m]
     else getBase5 d ++ [m]

getReducedBase5' :: [Integer] -> [Integer]
getReducedBase5' [] = []
getReducedBase5' xs =
  let l = last xs
      rs = init xs
  in if l <= 2 then getReducedBase5' rs ++ [l]
     else let l' = l - 5
          in case rs of
             [] -> 1 : [l']
             rs' -> let a = last rs'
                        b = init rs'
                        a' = a + 1
                    in getReducedBase5' (b ++ [a']) ++ [l']

getReducedBase5 :: Integer -> [Integer]
getReducedBase5 = getReducedBase5' . getBase5

showReducedBase5 :: [Integer] -> String
showReducedBase5 xs = map getCh xs
  where getCh 0 = '0'
        getCh 1 = '1'
        getCh 2 = '2'
        getCh (-1) = '-'
        getCh (-2) = '='
        getCh _ = undefined

readReducedBase5 :: String -> [Integer]
readReducedBase5 xs = map getNum xs
  where getNum '0' = 0
        getNum '1' = 1
        getNum '2' = 2
        getNum '-' = -1
        getNum '=' = -2
        getNum _ = undefined

getDecimal' :: Integer -> [Integer] -> Integer
getDecimal' _ [] = 0
getDecimal' p xs = getDecimal' (p+1) (init xs) + last xs * (5^p)

getDecimal :: [Integer] -> Integer
getDecimal xs = getDecimal' 0 xs

day25 :: IO ()
day25 = do
  -- let input = lines testInput
  input <- lines <$> readFile "input/Day25.txt"

  putStrLn "day25"

  let total = sum . map (getDecimal . readReducedBase5) $ input
      totalSnafu = showReducedBase5 . getReducedBase5 $ total
  print total
  putStrLn totalSnafu

  return ()
