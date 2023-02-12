{-# LANGUAGE ScopedTypeVariables #-}

module UtilVectorSpec where

import qualified Data.Vector as V

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified UtilVector as UV

utilVectorSpec :: Spec
utilVectorSpec = do

  describe "shift" $ do
    prop "should return same list if offset is zero" $ \(xs :: [Int], i) -> do
      vec <- V.thaw . V.fromList $ xs
      UV.shift 0 i vec
      xs' <- V.toList <$> V.freeze vec

      xs' `shouldBe` xs
