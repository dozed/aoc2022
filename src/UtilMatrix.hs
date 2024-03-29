module UtilMatrix (getLastColumn, appendColumn, dropLastColumn) where

import Data.Matrix (Matrix)
import qualified Data.Matrix as MT
  
getLastColumn :: Matrix a -> [a]
getLastColumn m =
  let as = MT.toLists m
      xs = map last as
  in xs

appendColumn :: a -> Matrix a -> Matrix a
appendColumn a m =
  let as = MT.toLists m
      as' = map (\xs -> xs ++ [a]) as
      m' = MT.fromLists as'
  in m'

dropLastColumn :: Matrix a -> Matrix a
dropLastColumn m =
  let as = MT.toLists m
      as' = map (\xs -> init xs) as
      m' = MT.fromLists as'
  in m'

