module NaturalsSpec where

import qualified Naturals as SUT
import Test.Hspec

import Naturals (Nat(..))

spec :: Spec

spec = do 
  describe "conversion" $ do
    it "should create Nat" $ do
      SUT.integerToNat 2 `shouldBe` Just (Succ (Succ Zero))
      SUT.integerToNat 0 `shouldBe` Just Zero
      SUT.integerToNat (-1) `shouldBe` Nothing

    it "should map to Integers" $ do
      SUT.natToInteger Zero `shouldBe` 0
      SUT.natToInteger (Succ (Succ (Succ Zero))) `shouldBe` 3
      