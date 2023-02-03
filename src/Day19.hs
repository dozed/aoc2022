{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day19 where

import Control.Logger.Simple (LogConfig(..), withGlobalLogging, logInfo, showText)
import Control.Monad (forM_, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Semigroup (stimes)
import Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)
import Text.RawString.QQ

import Util (lstrip, regularParse)

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
             deriving (Eq, Ord, Show)

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

getStartInventory :: Inventory
getStartInventory =
  Inventory {
    oreAmount = 0,
    clayAmount = 0,
    obsidianAmount = 0,
    geodeAmount = 0,
    numOreRobots = 1,
    numClayRobots = 0,
    numObsidianRobots = 0,
    numGeodeRobots = 0
  }

collectMaterials :: Inventory -> Inventory
collectMaterials i =
  i {
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

getRobotCost :: Blueprint -> RobotType -> RobotCost
getRobotCost bp OreRobot = oreRobotCost bp
getRobotCost bp ClayRobot = clayRobotCost bp
getRobotCost bp ObsidianRobot = obsidianRobotCost bp
getRobotCost bp GeodeRobot = geodeRobotCost bp

addRobotToInventory' :: RobotType -> Inventory -> Inventory
addRobotToInventory' OreRobot i = i { numOreRobots = numOreRobots i + 1 }
addRobotToInventory' ClayRobot i = i { numClayRobots = numClayRobots i + 1 }
addRobotToInventory' ObsidianRobot i = i { numObsidianRobots = numObsidianRobots i + 1 }
addRobotToInventory' GeodeRobot i = i { numGeodeRobots = numGeodeRobots i + 1 }

subtractRobotCostFromInventory :: RobotCost -> Inventory -> Inventory
subtractRobotCostFromInventory rc i =
  i {
    oreAmount = oreAmount i - oreCost rc,
    clayAmount = clayAmount i - clayCost rc,
    obsidianAmount = obsidianAmount i - obsidianCost rc
  }

addRobotToInventory :: Blueprint -> RobotType -> Inventory -> Inventory
addRobotToInventory bp rt i =
  let i' = addRobotToInventory' rt i
      rc = getRobotCost bp rt
      i'' = subtractRobotCostFromInventory rc i'
  in i''

isBuildable' :: Inventory -> RobotCost -> Bool
isBuildable' i rc = oreAmount i >= oreCost rc && clayAmount i >= clayCost rc && obsidianAmount i >= obsidianCost rc

getAllRobotTypes :: [RobotType]
getAllRobotTypes = [OreRobot, ClayRobot, ObsidianRobot, GeodeRobot]

data BuildRobot = BuildNoRobot | BuildRobot RobotType
                  deriving (Eq, Show)

showBuildRobotShort :: BuildRobot -> String
showBuildRobotShort BuildNoRobot = "n"
showBuildRobotShort (BuildRobot OreRobot) = "or"
showBuildRobotShort (BuildRobot ClayRobot) = "c"
showBuildRobotShort (BuildRobot ObsidianRobot) = "ob"
showBuildRobotShort (BuildRobot GeodeRobot) = "g"

getBuildableRobotTypes :: Blueprint -> Inventory -> [RobotType]
getBuildableRobotTypes bp i = [rt | rt <- getAllRobotTypes, isBuildable' i (getRobotCost bp rt)]

getLargestRobotTypeInInventory :: Inventory -> RobotType
getLargestRobotTypeInInventory i
  | numGeodeRobots i > 0 = GeodeRobot
  | numObsidianRobots i > 0 = ObsidianRobot
  | numClayRobots i > 0 = ClayRobot
  | otherwise = OreRobot

type Timestep = Int

search :: Blueprint -> Inventory -> Timestep -> IORef Int -> [BuildRobot] -> IO ()
search _ i 25 maxGeodesRef brs = do
  maxGeodes <- readIORef maxGeodesRef
  let currentGeodes = geodeAmount i
  when (currentGeodes > maxGeodes) $ do
    logInfo $ "- Geodes: " <> showText currentGeodes <> " - " <> showText (reverse . map showBuildRobotShort $ brs)
    writeIORef maxGeodesRef currentGeodes
search bp i ts maxGeodesRef brs = do
  let -- build new robots
      buildableRobots' = getBuildableRobotTypes bp i
      -- robots collect materials
      i' = collectMaterials i
      -- robots are built
      inventories = (BuildNoRobot, i') : map (\rt -> (BuildRobot rt, addRobotToInventory bp rt i')) buildableRobots'
  forM_ inventories $ \(br, i'') -> do
    search bp i'' (ts+1) maxGeodesRef (br:brs)

day19 :: IO ()
day19 = withGlobalLogging (LogConfig Nothing True) $ do
  -- let input = testInput
  input <- readFile "input/Day19.txt"

  blueprints <- case regularParse blueprintsParser input of
    Left e -> fail $ show e
    Right xs -> pure xs

  forM_ blueprints $ \bp -> do
    logInfo $ "Blueprint: " <> showText (blueprintId bp)
    maxGeodesRef <- newIORef 0
    search bp getStartInventory 1 maxGeodesRef []

  logInfo "Finished"
