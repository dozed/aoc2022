{-# LANGUAGE QuasiQuotes #-}

module Day19 where

import Control.Monad (void)
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ

import Util (lstrip)

testInput = lstrip [r|
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
|]

blueprintParser :: Parser Blueprint
blueprintParser = do
  void $ string "Blueprint "
  blueprintId <- int
  void $ string ": Each ore robot costs "
  oreCost1 <- int
  void $ string " ore. Each clay robot costs "
  oreCost2 <- int
  void $ string " ore. Each obsidian robot costs "
  oreCost3 <- int
  void $ string " ore and "
  clayCost3 <- int
  void $ string " clay. Each geode robot costs "
  oreCost4 <- int
  void $ string " ore and "
  obsidianCost4 <- int
  void $ string " obsidian."
  let oreRobotCosts = RobotCosts { oreCost = oreCost1, clayCost = 0, obsidianCost = 0 }
      clayRobotCosts = RobotCosts { oreCost = oreCost2, clayCost = 0, obsidianCost = 0 }
      obsidianRobotCosts = RobotCosts { oreCost = oreCost3, clayCost = clayCost3, obsidianCost = 0 }
      geodeRobotCosts = RobotCosts { oreCost = oreCost4, clayCost = 0, obsidianCost = obsidianCost4 }
      blueprint = Blueprint {
        blueprintId = blueprintId,
        oreRobotCosts = oreRobotCosts,
        clayRobotCosts = clayRobotCosts,
        obsidianRobotCosts = obsidianRobotCosts,
        geodeRobotCosts = geodeRobotCosts
      }
  return blueprint

blueprintsParser :: Parser [Blueprint]
blueprintsParser = endBy1 blueprintParser endOfLine

data Robot = Ore | Clay | Obsidian | Geode
             deriving (Eq, Show)

type Amount = Int
type Number = Int

data Inventory = Inventory {
  oreAmount :: Amount,
  clayAmount :: Amount,
  obsidianAmount :: Amount,
  geodeAmount :: Amount,
  numOreRobots :: Number,
  numClayRobots :: Number,
  numObsidianRobots :: Number,
  numGeodeRobots :: Number
} deriving (Show, Eq)

collectMaterials :: Inventory -> Inventory
collectMaterials i = i {
      oreAmount = oreAmount i + numOreRobots i,
      clayAmount = clayAmount i + numClayRobots i,
      obsidianAmount = obsidianAmount i + numObsidianRobots i,
      geodeAmount = geodeAmount i + numGeodeRobots i
    }

type Cost = Int

data RobotCosts = RobotCosts {
  oreCost :: Cost,
  clayCost :: Cost,
  obsidianCost :: Cost
} deriving (Eq, Show)

data Blueprint = Blueprint {
  blueprintId :: Int,
  oreRobotCosts :: RobotCosts,
  clayRobotCosts :: RobotCosts,
  obsidianRobotCosts :: RobotCosts,
  geodeRobotCosts :: RobotCosts
} deriving (Eq, Show)

day19 :: IO ()
day19 = do
  putStrLn "day19"
