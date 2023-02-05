module Day20Spec (day20Spec) where

import Test.Hspec

import Day20
import Util (regularParse)

day20Spec :: Spec
day20Spec = do

  describe "parseNumbers" $ do
    it "should parse numbers" $ do
      numbers <- case regularParse parseNumbers testInput of
        Left e -> fail $ show e
        Right xs -> pure xs

      numbers `shouldBe` [1, 2, -3, 3, -2, 0, 4]

  describe "applyOffset" $ do
    it "should compute a new index by applying an offset to an index in a cyclic list" $ do
      applyOffset 5 2 13 `shouldBe` 0
      applyOffset 5 2 3 `shouldBe` 0

      applyOffset 7 0 1 `shouldBe` 1
      applyOffset 7 0 2 `shouldBe` 2
      applyOffset 7 4 (-2) `shouldBe` 2
      applyOffset 7 1 (-3) `shouldBe` 5

      applyOffset 8 2 7 `shouldBe` 1
      applyOffset 8 2 (-3) `shouldBe` 7
      applyOffset 8 2 (-13) `shouldBe` 5
      applyOffset 6 3 (-2) `shouldBe` 1
      applyOffset 7 6 4 `shouldBe` 3
      applyOffset 7 3 2 `shouldBe` 5
      applyOffset 7 3 (-2) `shouldBe` 1

  describe "mix" $ do
    it "should move an element by a specific offset" $ do
      let xs1 = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
      let xs2 = mix xs1 (IdInt 0 1)
      let xs3 = mix xs2 (IdInt 1 2)
      let xs4 = mix xs3 (IdInt 2 (-3))
      let xs5 = mix xs4 (IdInt 3 3)
      let xs6 = mix xs5 (IdInt 4 (-2))
      let xs7 = mix xs6 (IdInt 5 0)
      let xs8 = mix xs7 (IdInt 6 4)

      xs2 `shouldBe` [IdInt 1 2, IdInt 0 1, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
      xs3 `shouldBe` [IdInt 0 1, IdInt 2 (-3), IdInt 1 2, IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
      xs4 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 3 3, IdInt 4 (-2), IdInt 2 (-3), IdInt 5 0, IdInt 6 4]
      xs5 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 4 (-2), IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4]
      xs6 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4, IdInt 4 (-2)]
      xs7 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4, IdInt 4 (-2)]
      xs8 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]
