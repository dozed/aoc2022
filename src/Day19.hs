{-# LANGUAGE QuasiQuotes #-}

module Day19 where

import Control.Monad (void)
import Data.Semigroup (stimes)
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
  let oreRobotCosts = RobotCost { oreCost = oreCost1, clayCost = 0, obsidianCost = 0 }
      clayRobotCosts = RobotCost { oreCost = oreCost2, clayCost = 0, obsidianCost = 0 }
      obsidianRobotCosts = RobotCost { oreCost = oreCost3, clayCost = clayCost3, obsidianCost = 0 }
      geodeRobotCosts = RobotCost { oreCost = oreCost4, clayCost = 0, obsidianCost = obsidianCost4 }
      blueprint = Blueprint {
        blueprintId = blueprintId,
        oreRobotCost = oreRobotCosts,
        clayRobotCost = clayRobotCosts,
        obsidianRobotCost = obsidianRobotCosts,
        geodeRobotCost = geodeRobotCosts
      }
  return blueprint

blueprintsParser :: Parser [Blueprint]
blueprintsParser = endBy1 blueprintParser endOfLine

data RobotType = OreRobot | ClayRobot | ObsidianRobot | GeodeRobot
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

data RobotCost = RobotCost {
  oreCost :: Cost,
  clayCost :: Cost,
  obsidianCost :: Cost
} deriving (Eq, Show)

instance Semigroup RobotCost where
  (<>) rc1 rc2 =
      RobotCost {
        oreCost = oreCost rc1 + oreCost rc2,
        clayCost = clayCost rc1 + clayCost rc2,
        obsidianCost = obsidianCost rc1 + obsidianCost rc2
      }

  stimes n rc =
    RobotCost {
      oreCost = oreCost rc * fromIntegral n,
      clayCost = clayCost rc * fromIntegral n,
      obsidianCost = obsidianCost rc * fromIntegral n
    }

instance Monoid RobotCost where
  mempty = RobotCost { oreCost = 0, clayCost = 0, obsidianCost = 0 }
  mappend = (<>)

data Blueprint = Blueprint {
  blueprintId :: Int,
  oreRobotCost :: RobotCost,
  clayRobotCost :: RobotCost,
  obsidianRobotCost :: RobotCost,
  geodeRobotCost :: RobotCost
} deriving (Eq, Show)

getMaximumBuildableRobots' :: RobotCost -> Inventory -> Int
getMaximumBuildableRobots' rc i =
  let maxByOre = if oreCost rc == 0 then 0 else oreAmount i `div` oreCost rc
      maxByClay = if clayCost rc == 0 then 0 else clayAmount i `div` clayCost rc
      maxByObsidian = if obsidianCost rc == 0 then 0 else obsidianAmount i `div` obsidianCost rc
  in minimum [maxByOre, maxByClay, maxByObsidian]

getMaximumBuildableRobots :: Blueprint -> Inventory -> RobotType -> Int
getMaximumBuildableRobots bp i OreRobot = getMaximumBuildableRobots' (oreRobotCost bp) i
getMaximumBuildableRobots bp i ClayRobot = getMaximumBuildableRobots' (clayRobotCost bp) i
getMaximumBuildableRobots bp i ObsidianRobot = getMaximumBuildableRobots' (obsidianRobotCost bp) i
getMaximumBuildableRobots bp i GeodeRobot = getMaximumBuildableRobots' (geodeRobotCost bp) i

data RobotBuild = RobotBuild {
  buildOreRobots :: Number,
  buildClayRobots :: Number,
  buildObsidianRobots :: Number,
  buildGeodeRobots :: Number
} deriving (Eq, Show)

addRobotsToInventory :: RobotType -> Number -> Inventory -> Inventory
addRobotsToInventory OreRobot n i = i { numOreRobots = numOreRobots i + n }
addRobotsToInventory ClayRobot n i = i { numClayRobots = numClayRobots i + n }
addRobotsToInventory ObsidianRobot n i = i { numObsidianRobots = numObsidianRobots i + n }
addRobotsToInventory GeodeRobot n i = i { numGeodeRobots = numGeodeRobots i + n }

isBuildable :: Blueprint -> Inventory -> RobotBuild -> Bool
isBuildable bp i rb =
  let oreCosts = stimes (buildOreRobots rb) (oreRobotCost bp)
      clayCosts = stimes (buildClayRobots rb) (clayRobotCost bp)
      obsidianCosts = stimes (buildObsidianRobots rb) (obsidianRobotCost bp)
      robotsCosts = mconcat [oreCosts, clayCosts, obsidianCosts]
  in oreAmount i >= oreCost robotsCosts && clayAmount i >= clayCost robotsCosts && obsidianAmount i >= obsidianCost robotsCosts

getBuildableRobots :: Blueprint -> Inventory -> [RobotBuild]
getBuildableRobots bp i = do
  let maxOreRobots = getMaximumBuildableRobots bp i OreRobot
      maxClayRobots = getMaximumBuildableRobots bp i ClayRobot
      maxObsidianRobots = getMaximumBuildableRobots bp i ObsidianRobot
      maxGeodeRobots = getMaximumBuildableRobots bp i GeodeRobot

  numOreRobots <- [0..maxOreRobots]
  numClayRobots <- [0..maxClayRobots]
  numObsidianRobots <- [0..maxObsidianRobots]
  numGeodeRobots <- [0..maxGeodeRobots]

  let build = RobotBuild {
    buildOreRobots = numOreRobots,
    buildClayRobots = numClayRobots,
    buildObsidianRobots = numObsidianRobots,
    buildGeodeRobots = numGeodeRobots
  }

  build' <- if isBuildable bp i build then [build] else []

  return build'

day19 :: IO ()
day19 = do
  putStrLn "day19"
