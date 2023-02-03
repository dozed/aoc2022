module Day19Spec where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

import Day19
import Util (regularParse)

instance Arbitrary RobotCost where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return RobotCost { oreCost = a, clayCost = b, obsidianCost = c }

instance EqProp RobotCost where
  (=-=) = eq

day19Spec :: Spec
day19Spec = do

  describe "RobotCost" $ do
    testBatch (semigroup (undefined :: RobotCost, undefined :: Int))
    testBatch (monoid (undefined :: RobotCost))

  describe "blueprintsParser" $ do
    it "should parse a list of Blueprint" $ do
      let oreRobotCosts1 = RobotCost { oreCost = 4, clayCost = 0, obsidianCost = 0 }
          clayRobotCosts1 = RobotCost { oreCost = 2, clayCost = 0, obsidianCost = 0 }
          obsidianRobotCosts1 = RobotCost { oreCost = 3, clayCost = 14, obsidianCost = 0 }
          geodeRobotCosts1 = RobotCost { oreCost = 2, clayCost = 0, obsidianCost = 7 }
          blueprint1 = Blueprint { blueprintId = 1, oreRobotCost = oreRobotCosts1, clayRobotCost = clayRobotCosts1, obsidianRobotCost = obsidianRobotCosts1, geodeRobotCost = geodeRobotCosts1 }

      let oreRobotCosts2 = RobotCost { oreCost = 2, clayCost = 0, obsidianCost = 0 }
          clayRobotCosts2 = RobotCost { oreCost = 3, clayCost = 0, obsidianCost = 0 }
          obsidianRobotCosts2 = RobotCost { oreCost = 3, clayCost = 8, obsidianCost = 0 }
          geodeRobotCosts2 = RobotCost { oreCost = 3, clayCost = 0, obsidianCost = 12 }
          blueprint2 = Blueprint { blueprintId = 2, oreRobotCost = oreRobotCosts2, clayRobotCost = clayRobotCosts2, obsidianRobotCost = obsidianRobotCosts2, geodeRobotCost = geodeRobotCosts2 }

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
