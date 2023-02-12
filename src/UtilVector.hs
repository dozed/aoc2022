module UtilVector where

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV

type Index = Int
type Offset = Int

elemIndex :: Eq a => IOVector a -> a -> Index -> IO (Maybe Int)
elemIndex vec _ i | i == MV.length vec = return Nothing
elemIndex vec a i = do
  a' <- MV.read vec i
  if a' == a then return (Just i)
  else elemIndex vec a (i+1)

swap :: Int -> Int -> IOVector a -> IO ()
swap from to vec = do
  x <- MV.read vec from
  y <- MV.read vec to
  MV.write vec from y
  MV.write vec to x
  return ()

shift :: Offset -> Index -> IOVector a -> IO ()
shift 0 _ _ = return ()
shift offset from vec = do
  let len = MV.length vec
      to = if offset > 0 then
             if from == len - 1 then 0 else from + 1
           else
             if from == 0 then len - 1 else from - 1
      offset' = if offset > 0 then offset - 1
                else offset + 1
  swap from to vec
  shift offset' to vec

