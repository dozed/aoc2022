{-# LANGUAGE NamedFieldPuns #-}

module UtilViterbi where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Matrix (Matrix)
import qualified Data.Matrix as MT

import UtilMatrix (appendColumn)

type Timestep = Int

data ViterbiInfo a b = ViterbiInfo {
  values :: [a],
  indexes :: Map a Int,
  distances :: Matrix Int,
  start :: a,
  mkEmpty :: (a, b),
  getCandidates :: ViterbiInfo a b -> Matrix (a, b) -> Timestep -> a -> [a],
  getB :: ViterbiInfo a b -> Timestep -> a -> a -> b,
  isFinished :: ViterbiInfo a b -> Matrix (a, b) -> Timestep -> Bool
}

viterbiStepForValue :: (Ord a, Ord b) => ViterbiInfo a b -> Matrix (a, b) -> Int -> a -> Matrix (a, b)
viterbiStepForValue info@ViterbiInfo { indexes, start, getB } previousValves timestep@1 toValve =
  let valveIdx = indexes M.! toValve
      (maxPrev, b) = (start, getB info timestep start toValve)
      previousValves' = MT.setElem (maxPrev, b) (valveIdx, timestep) previousValves
  in previousValves'
viterbiStepForValue info@ViterbiInfo { indexes, getCandidates, getB } previousValves timestep toValve =
  let valveIdx = indexes M.! toValve
      candidates = getCandidates info previousValves timestep toValve
  in if null candidates then previousValves  -- some valves dont have previous values -> paths ends
     else
       let pprsRemainings = map (\fromValve -> getB info timestep fromValve toValve) candidates
           xs = candidates `zip` pprsRemainings
           (maxPrev, b) = maximumBy (compare `on` snd) xs
           -- map of previous valve at a given timestep for a given valve with a tag, e.g. remaining minutes and released pressure
           previousValves' = MT.setElem (maxPrev, b) (valveIdx, timestep) previousValves
       in previousValves'

viterbiRound :: (Ord a, Ord b) => ViterbiInfo a b -> Matrix (a, b) -> Int -> Matrix (a, b)
viterbiRound info@ViterbiInfo { start, values } previousValves timestep =
  let valveLabels' = filter (/= start) values
  in foldl (\pv v -> viterbiStepForValue info pv timestep v) previousValves valveLabels'

viterbi :: (Ord a, Show a, Ord b, Show b) => ViterbiInfo a b -> Matrix (a, b) -> Int -> IO (Matrix (a, b))
viterbi info@ViterbiInfo { indexes, mkEmpty, isFinished } previousValves timestep = do
  putStrLn $ "timestep: " <> show timestep
  print indexes
  let previousValves' = appendColumn mkEmpty previousValves
      previousValves'' = viterbiRound info previousValves' timestep
  print previousValves''
  putStrLn "-----"
  if isFinished info previousValves'' timestep then return previousValves''
  else viterbi info previousValves'' (timestep + 1)

-- getAccPPRAndRemaining fromValve toValve =
--   let fromIdx = indexes M.! fromValve
--       fromRemaining = remainingMinutes MT.! (fromIdx, timestep - 1)
--       (curPpr, currentRemaining) = getPotentialPressureRelease info (fromValve, fromRemaining) toValve
--       prevPpr = pprs MT.! (fromIdx, timestep - 1)
--   in (prevPpr + curPpr, currentRemaining)
--
-- getPathTo :: Matrix Label -> Map Label Int -> Label -> Int -> [Label]
-- getPathTo _ _ x 0 = [x]
-- getPathTo previousValves valveIdxs valve i =
--   let valveIdx = valveIdxs M.! valve
--       x = previousValves MT.! (valveIdx, i)
--   in valve : getPathTo previousValves valveIdxs x (i-1)
--
-- getCandidates :: [Label] -> Map Label Int -> Matrix Label -> Int -> Label -> [Label]
-- getCandidates _ _ _ 0 _ = ["AA"]
-- getCandidates valveLabels valveIdxs previousValves pathLength valve =
--   let candidates = filter (\v -> v /= valve && v /= "AA" && previousValves MT.! (valveIdxs M.! v, pathLength - 1) /= "--") valveLabels
--       -- the same valve cant be the previous valve
--       paths = map (\v -> getPathTo previousValves valveIdxs v (pathLength - 1)) candidates
--       -- a previous valve which contains valve on its path cant be the previous valve
--       candidates' = map fst . filter (\(c, p) -> valve `notElem` p) $ (candidates `zip` paths)
--   in candidates'
