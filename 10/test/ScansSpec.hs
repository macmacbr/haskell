module ScansSpec where

import qualified Scans as SUT
import Test.Hspec

spec :: Spec
spec = do
  describe "fibs" $ do
    it "should create valid fibbonacci sequence" $
      take 8 SUT.fibs `shouldBe` [1,1,2,3,5,8,13,21]
      
  describe "fibsN" $ do
    it "should calculate the nTh fibonacci number" $
      SUT.fibsN 10 `shouldBe` 89
      
  describe "factorial" $ do
    it "should calculate the factorial sequence" $
      take 6 SUT.factorial `shouldBe` [1,2,6,24,120,720]
  
  describe "factorial1" $ do
    it "should calculate the factorial sequence" $
      take 6 SUT.factorial1 `shouldBe` [1,2,6,24,120,720]      