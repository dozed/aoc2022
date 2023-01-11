{-# LANGUAGE NamedFieldPuns #-}

module UtilViterbi (ViterbiInfo(..), Timestep, viterbi) where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Matrix (Matrix)
import qualified Data.Matrix as MT

import UtilMatrix (appendColumn)

type Timestep = Int

data ViterbiInfo a b = ViterbiInfo {
  -- | The possible `a` values
  values :: [a],
  -- | The indexes of the `a`'s
  indexes :: Map a Int,
  -- | The start value for each sequence
  mkStart :: a,
  -- | Used to fill an empty column when a new Viterbi iteration starts
  mkEmpty :: (a, b),
  -- | Get possible previous candidates for the `a` at the current timestep
  -- There might be not all `a`'s possible candidates, e.g. if they exist on the current path already
  getCandidates
    :: ViterbiInfo a b  -- ^ The info about the structure Viterbi is applied to
    -> Matrix (a, b)  -- ^ The current matrix of tagged, previous values
    -> Timestep  -- ^ The current timestep
    -> a  -- ^ The value for which the candidate should be found
    -> [a],  -- ^ The list of candidates for that value at the current timestep
  -- | Get a `b` given from/to `a`s
  getB
    :: ViterbiInfo a b  -- ^ The info about the structure Viterbi is applied to
    -> Matrix (a, b)  -- ^ The current matrix of tagged, previous values
    -> Timestep  -- ^ The current timestep
    -> a  -- ^ The `from` value for which the `b` should be computed
    -> a  -- ^ The `to` value for which the `b` should be computed
    -> b,  -- ^ The `b` for the current edge
  isFinished :: ViterbiInfo a b -> Matrix (a, b) -> Timestep -> Bool
}

viterbiStepForValue :: (Ord a, Ord b) => ViterbiInfo a b -> Matrix (a, b) -> Int -> a -> Matrix (a, b)
viterbiStepForValue info@ViterbiInfo { indexes, mkStart, getB } previous timestep@1 to =
  let toIdx = indexes M.! to
      (maxPrev, b) = (mkStart, getB info previous timestep mkStart to)
      previous' = MT.setElem (maxPrev, b) (toIdx, timestep) previous
  in previous'
viterbiStepForValue info@ViterbiInfo { indexes, getCandidates, getB } previous timestep to =
  let toIdx = indexes M.! to
      candidates = getCandidates info previous timestep to
  in if null candidates then previous  -- some nodes dont have previous values -> paths ends
     else
       let bs = map (\from -> getB info previous timestep from to) candidates
           xs = candidates `zip` bs
           (maxPrev, b) = maximumBy (compare `on` snd) xs
           -- map of previous nodes at a given timestep for a given node with a tag, e.g. remaining minutes and released pressure
           previous' = MT.setElem (maxPrev, b) (toIdx, timestep) previous
       in previous'

viterbiRound :: (Ord a, Ord b) => ViterbiInfo a b -> Matrix (a, b) -> Int -> Matrix (a, b)
viterbiRound info@ViterbiInfo { mkStart, values } previous timestep =
  let values' = filter (/= mkStart) values
  in foldl (\pv v -> viterbiStepForValue info pv timestep v) previous values'

viterbi' :: (Ord a, Show a, Ord b, Show b) => ViterbiInfo a b -> Matrix (a, b) -> Int -> Matrix (a, b)
viterbi' info@ViterbiInfo { mkEmpty, isFinished } previous timestep =
  let previous' = appendColumn mkEmpty previous
      previous'' = viterbiRound info previous' timestep
      previous''' = if isFinished info previous'' timestep then previous''
                    else viterbi' info previous'' (timestep + 1)
  in previous'''

viterbi :: (Ord a, Show a, Ord b, Show b) => ViterbiInfo a b -> Matrix (a, b)
viterbi info@ViterbiInfo { values } =
  let previous = MT.fromLists $ replicate (length values - 1) []
      previous' = viterbi' info previous 1
  in previous'

