module UtilMatrixSpec where

import Data.Matrix (Matrix)
import qualified Data.Matrix as MT

import Test.Hspec

import UtilMatrix

utilMatrixSpec :: Spec
utilMatrixSpec = do

  describe "appendColumn" $ do
    it "should append a column with a default value to a matrix" $ do
      let m = MT.fromLists [[2], [3]]

      appendColumn 0 m `shouldBe` MT.fromLists [[2, 0], [3, 0]]

  describe "getLastColumn" $ do
    it "should return the last column of a matrix" $ do
      let m = MT.fromLists [[1, 2], [3, 4]]

      getLastColumn m `shouldBe` [2, 4]

  describe "dropLastColumn" $ do
    it "should drop the last column of a matrix" $ do
      let m = MT.fromLists [[1, 2], [3, 4]]

      dropLastColumn m `shouldBe` MT.fromLists [[1], [3]]
