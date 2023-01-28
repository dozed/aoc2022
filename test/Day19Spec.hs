module Day19Spec where

import Test.Hspec

import Day19
import Util (regularParse)

day19Spec :: Spec
day19Spec = do

  describe "blueprintsParser" $ do
    it "should parse a list of Blueprint" $ do
      let oreRobotCosts1 = RobotCosts { oreCost = 4, clayCost = 0, obsidianCost = 0 }
          clayRobotCosts1 = RobotCosts { oreCost = 2, clayCost = 0, obsidianCost = 0 }
          obsidianRobotCosts1 = RobotCosts { oreCost = 3, clayCost = 14, obsidianCost = 0 }
          geodeRobotCosts1 = RobotCosts { oreCost = 2, clayCost = 0, obsidianCost = 7 }
          blueprint1 = Blueprint { blueprintId = 1, oreRobotCosts = oreRobotCosts1, clayRobotCosts = clayRobotCosts1, obsidianRobotCosts = obsidianRobotCosts1, geodeRobotCosts = geodeRobotCosts1 }

      let oreRobotCosts2 = RobotCosts { oreCost = 2, clayCost = 0, obsidianCost = 0 }
          clayRobotCosts2 = RobotCosts { oreCost = 3, clayCost = 0, obsidianCost = 0 }
          obsidianRobotCosts2 = RobotCosts { oreCost = 3, clayCost = 8, obsidianCost = 0 }
          geodeRobotCosts2 = RobotCosts { oreCost = 3, clayCost = 0, obsidianCost = 12 }
          blueprint2 = Blueprint { blueprintId = 2, oreRobotCosts = oreRobotCosts2, clayRobotCosts = clayRobotCosts2, obsidianRobotCosts = obsidianRobotCosts2, geodeRobotCosts = geodeRobotCosts2 }

      let expectedBlueprints = [blueprint1, blueprint2]

      regularParse blueprintsParser testInput `shouldBe` Right expectedBlueprints

  describe "collectMaterials" $ do
    it "should add new materials" $ do
      let inventory = Inventory {
        oreAmount = 1,
        clayAmount = 2,
        obsidianAmount = 3,
        geodeAmount = 4,
        numOreRobots = 5,
        numClayRobots = 6,
        numObsidianRobots = 7,
        numGeodeRobots = 8
      }

      let expected = Inventory {
        oreAmount = 6,
        clayAmount = 8,
        obsidianAmount = 10,
        geodeAmount = 12,
        numOreRobots = 5,
        numClayRobots = 6,
        numObsidianRobots = 7,
        numGeodeRobots = 8
      }

      collectMaterials inventory `shouldBe` expected
