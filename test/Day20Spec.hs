{-# LANGUAGE ScopedTypeVariables #-}

module Day20Spec (day20Spec) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Day20
import Util (regularParse)

compareNumbers :: [IdInt] -> [IdInt] -> Expectation
compareNumbers actual expected = do
  let nc = length actual
      ne = length expected
      cycleResetTake n as = take n . dropWhile (\(IdInt _ i) -> i /= 0) . cycle $ as
      current' = cycleResetTake nc actual
      expected' = cycleResetTake nc expected

  nc `shouldBe` ne
  current' `shouldBe` expected'

day20Spec :: Spec
day20Spec = do

  describe "parseInts" $ do
    it "should parse numbers" $ do
      numbers <- case regularParse parseInts testInput of
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

  describe "mixOne" $ do
    it "should move an element by a specific offset" $ do
      let xs = [IdInt 0 2, IdInt 1 7, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          expected = [IdInt 0 2, IdInt 2 1, IdInt 3 3, IdInt 1 7, IdInt 4 5]

      let actual = mixOne xs (IdInt 1 7)

      actual `shouldBe` expected

    it "should move elements by a specific offset" $ do
      let xs1 = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          xs2 = mixOne xs1 (IdInt 0 1)
          xs3 = mixOne xs2 (IdInt 1 2)
          xs4 = mixOne xs3 (IdInt 2 (-3))
          xs5 = mixOne xs4 (IdInt 3 3)
          xs6 = mixOne xs5 (IdInt 4 (-2))
          xs7 = mixOne xs6 (IdInt 5 0)
          xs8 = mixOne xs7 (IdInt 6 4)

      xs2 `shouldBe` [IdInt 1 2, IdInt 0 1, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
      xs3 `shouldBe` [IdInt 0 1, IdInt 2 (-3), IdInt 1 2, IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
      xs4 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 3 3, IdInt 4 (-2), IdInt 2 (-3), IdInt 5 0, IdInt 6 4]
      xs5 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 4 (-2), IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4]
      xs6 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4, IdInt 4 (-2)]
      xs7 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 5 0, IdInt 3 3, IdInt 6 4, IdInt 4 (-2)]
      xs8 `shouldBe` [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

  describe "mix" $ do
    it "should mix a list of IdInt" $ do
      let idInts = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          expected = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

      mix idInts idInts `shouldBe` expected

  describe "shift" $ do
    prop "should return same list if offset is zero" $ \(xs :: [Int], i) -> do
      shift 0 i xs `shouldBe` xs

    it "should shift an element forward by a specified amount" $ do
      let xs = [2, 1, 3, 4, -1]

      shift 1 0 xs `shouldBe` [1, 2, 3, 4, -1]
      shift 2 0 xs `shouldBe` [1, 3, 2, 4, -1]
      shift 1 4 xs `shouldBe` [-1, 1, 3, 4, 2]
      shift 3 3 xs `shouldBe` [1, 4, 3, -1, 2]
      shift 13 3 xs `shouldBe` [-1, 4, 2, 1, 3]

    it "should shift an element backward by a specified amount" $ do
      let xs = [2, 1, 3, 4, -1]

      shift (-1) 4 xs `shouldBe` [2, 1, 3, -1, 4]
      shift (-2) 4 xs `shouldBe` [2, 1, -1, 3, 4]
      shift (-1) 0 xs `shouldBe` [-1, 1, 3, 4, 2]
      shift (-2) 0 xs `shouldBe` [-1, 1, 3, 2, 4]
      shift (-3) 1 xs `shouldBe` [-1, 2, 3, 1, 4]
      shift (-13) 1 xs `shouldBe` [3, 4, -1, 1, 2]

  describe "mix'" $ do
    it "should mix a list of IdInt" $ do
      let idInts = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          expected = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

      idInts' <- liftIO $ mix' idInts

      compareNumbers idInts' expected

  describe "mix''" $ do
    it "should mix a list of IdInt" $ do
      let idInts = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 3 3, IdInt 4 (-2), IdInt 5 0, IdInt 6 4]
          expected = [IdInt 0 1, IdInt 1 2, IdInt 2 (-3), IdInt 6 4, IdInt 5 0, IdInt 3 3, IdInt 4 (-2)]

      idInts' <- liftIO $ mix'' idInts idInts

      compareNumbers idInts' expected

  describe "mixOne and mixOne''" $ do
    it "should give same results for test input" $ do
      let idInts = [IdInt 0 2, IdInt 1 7, IdInt 2 1, IdInt 3 3, IdInt 4 5]
          expected = [IdInt 0 2, IdInt 2 1, IdInt 3 3, IdInt 1 7, IdInt 4 5]
          idInt = idInts !! 1

      vec <- liftIO $ V.thaw $ V.fromList idInts

      let idInts' = mixOne idInts idInt
      liftIO $ mixOne'' vec idInt

      idInts'' <- V.toList <$> V.freeze vec

      idInts' `shouldBe` idInts''
      -- compareNumbers idInts' idInts''

    it "should give same results for real input" $ do
      input <- liftIO $ readFile "input/Day20.txt"

      ints <- case regularParse parseInts input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let idInts = zipWith IdInt [0..] ints
          idInt = idInts !! 1

      vec <- liftIO $ V.thaw $ V.fromList idInts

      let idInts' = mixOne idInts idInt
      liftIO $ mixOne'' vec idInt

      idInts'' <- V.toList <$> V.freeze vec

      idInts' `shouldBe` idInts''
      -- compareNumbers idInts' idInts''

  describe "mixN and mixN''" $ do
    it "should give same results" $ do
      input <- liftIO $ readFile "input/Day20.txt"

      ints <- case regularParse parseInts input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let idInts = zipWith IdInt [0..] ints

      let idInts' = mixN 2 idInts idInts
      idInts'' <- liftIO $ mixN'' 2 idInts idInts

      -- compareNumbers idInts' idInts''
      idInts' `shouldBe` idInts''

  describe "mix and mix''" $ do
    it "should give same results" $ do
      input <- liftIO $ readFile "input/Day20.txt"

      ints <- case regularParse parseInts input of
        Left e -> fail $ show e
        Right xs -> pure xs

      let idInts = zipWith IdInt [0..] ints

      let idInts' = mix idInts idInts
      idInts'' <- liftIO $ mix'' idInts idInts

      compareNumbers idInts' idInts''
