module UtilGraphSearch where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Matrix (Matrix)
import qualified Data.Matrix as MT
import Data.Set (Set)
import qualified Data.Set as S

type Predecessors a = Map a a

getPath'' :: Ord a => Predecessors a -> a -> [a] -> [a]
getPath'' predecessors a path =
  case M.lookup a predecessors of
    Nothing -> a:path
    Just p -> getPath'' predecessors p (a:path)

getPath' :: Ord a => Predecessors a -> a -> [a]
getPath' predecessors a = getPath'' predecessors a []

getPath :: Ord a => Map a (Predecessors a) -> a -> a -> [a]
getPath predecessorsMap from to = case M.lookup from predecessorsMap of
  Nothing -> []
  Just m -> getPath' m to

getShortestPathLengths :: Ord a => [a] -> Map a (Predecessors a) -> Matrix Int
getShortestPathLengths valveLabels predecessorsMap =
  let xxs = [[length (getPath predecessorsMap x y) - 1 | x <- valveLabels] | y <- valveLabels]
      matrix = MT.fromLists xxs
  in matrix

bfs' :: Ord a => (a -> Set a) -> a -> [a] -> Set a -> Predecessors a -> Predecessors a
bfs' getNeighbours current toVisit visited predecessors =
  let visited' = S.insert current visited
      reachableNodes = getNeighbours current
      reachableNodesNotVisited = S.difference reachableNodes visited'
      reachableNodesNotVisitedAndNotToVisit = S.difference reachableNodesNotVisited (S.fromList toVisit)
      toVisit' = toVisit ++ S.toList reachableNodesNotVisitedAndNotToVisit
      predecessors' = foldl (\acc x -> M.insert x current acc) predecessors reachableNodesNotVisitedAndNotToVisit

  in case toVisit' of
    [] -> predecessors'
    x:xs -> bfs' getNeighbours x xs visited' predecessors'

bfs :: Ord a => (a -> Set a) -> a -> Predecessors a
bfs getNeighbours current = bfs' getNeighbours current [] S.empty M.empty
