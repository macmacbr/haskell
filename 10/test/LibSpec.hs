--file test/LibSpec.hs
module LibSpec where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "foldrf" $ do
    it "should fold an empty list" $ 
       foldrfr (+) 0 [] `shouldBe` 0
       
    it "should fold a single element list" $
       foldrfr (+) 0 [1] `shouldBe` 1
       
    it "should sum a list" $
       foldrfr (+) 0 [1,2,3] `shouldBe` 6
       
  describe "foldSilly" $ do
    it "should return Silly with a list input" $
      foldSilly [0] `shouldBe` "Silly"
      
    it "should return Silly with a defined list with undefined" $
      foldSilly [1,2,3,undefined] `shouldBe` "Silly"
      
    it "should return Silly with an undefined list spine" $
      foldSilly ([1,2,3] ++ undefined) `shouldBe` "Silly"
    