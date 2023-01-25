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
          blueprint1 = Blueprint { oreRobotCosts = oreRobotCosts1, clayRobotCosts = clayRobotCosts1, obsidianRobotCosts = obsidianRobotCosts1, geodeRobotCosts = geodeRobotCosts1 }

      let oreRobotCosts2 = RobotCosts { oreCost = 2, clayCost = 0, obsidianCost = 0 }
          clayRobotCosts2 = RobotCosts { oreCost = 3, clayCost = 0, obsidianCost = 0 }
          obsidianRobotCosts2 = RobotCosts { oreCost = 3, clayCost = 8, obsidianCost = 0 }
          geodeRobotCosts2 = RobotCosts { oreCost = 3, clayCost = 0, obsidianCost = 12 }
          blueprint2 = Blueprint { oreRobotCosts = oreRobotCosts2, clayRobotCosts = clayRobotCosts2, obsidianRobotCosts = obsidianRobotCosts2, geodeRobotCosts = geodeRobotCosts2 }

      let expectedBlueprints = [blueprint1, blueprint2]

      regularParse blueprintsParser testInput `shouldBe` Right expectedBlueprints
