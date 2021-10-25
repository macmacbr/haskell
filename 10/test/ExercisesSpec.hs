module ExercisesSpec where

import qualified Exercises as SUT

import Test.Hspec

spec :: Spec
spec = do
  describe "myOr" $ do
    it "should return True when one is found" $ do
      SUT.myOr [False, True, False] `shouldBe` True
      
    it "should return False when no True is found" $ do
      SUT.myOr [False, False, False] `shouldBe` False
    
  describe "myAny" $ do
    it "should return True when one is found" $ do
      SUT.myAny even [1,3,4,5] `shouldBe` True
      
    it "should return False when no True is found" $ do
      SUT.myAny even [3,5,7,9] `shouldBe` False
                  
  describe "myElem" $ do
     it "should detect the element in a collection" $ do
       SUT.myElem 3 [1,2,3,4,5] `shouldBe` True

     it "should detect the element is absent in a collection" $ do
       SUT.myElem 10 [1,2,3,4,5] `shouldBe` False                  

  describe "myElem1" $ do
     it "should detect the element in a collection" $ do
       SUT.myElem1 3 [1,2,3,4,5] `shouldBe` True

     it "should detect the element is absent in a collection" $ do
       SUT.myElem1 10 [1,2,3,4,5] `shouldBe` False

  describe "myReverse" $ do
    it "should revert a String" $ do
      SUT.myReverse "abcde" `shouldBe` "edcba"

    it "should revert numbers" $ do
      SUT.myReverse [1,2,3,4,5] `shouldBe` [5,4,3,2,1]

  describe "myMap" $ do
    it "should map and change all elements" $ do
      SUT.myMap (*2) [1,2,3,4] `shouldBe` [2,4,6,8]  
  
  describe "myFilter" $ do
    it "should filter only valid elements" $ do
      SUT.myFilter even [1,2,3,4] `shouldMatchList` [2,4]   
      
  describe "squish" $ do
    it "should flatmap that shit" $ do
      SUT.squish ["flat", "map", "that", "shit"] `shouldBe` "flatmapthatshit"

      
  describe "squishMap" $ do
    it "should map over list and concatenate results from lists" $ do
      SUT.squishMap (\x -> [1, x, 3]) [2] `shouldBe` [1,2,3]
   
    it "should do a WOOT dance" $ do
      SUT.squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah" `shouldBe` "WO b OT WO l OT WO a OT WO h OT "

  describe "squishMapPointless" $ do
    it "should map over list and concatenate results from lists" $ do
      SUT.squishMapPointless (\x -> [1, x, 3]) [2] `shouldBe` [1,2,3]

    it "should do a WOOT dance" $ do
      SUT.squishMapPointless (\x -> "WO " ++ [x] ++ " OT ") "blah" `shouldBe` "WO b OT WO l OT WO a OT WO h OT "

  describe "squishMap1" $ do
    it "should map over list and concatenate results from lists" $ do
      SUT.squishMap1 (\x -> [1, x, 3]) [2] `shouldBe` [1,2,3]
   
    it "should do a WOOT dance" $ do
      SUT.squishMap1 (\x -> "WO " ++ [x] ++ " OT ") "blah" `shouldBe` "WO b OT WO l OT WO a OT WO h OT "

      
  describe "squishAgain" $ do
    it "should flatmap that shit" $ do
      SUT.squishAgain ["flat", "map", "that", "shit"] `shouldBe` "flatmapthatshit"   
      
  describe "myMaximumBy" $ do
    it "should return the head when all is 'great'" $ do  
      SUT.myMaximumBy (\_ _ -> GT) [1..10] `shouldBe` 1
    
    it "should return the last when more is 'less'" $ do  
      SUT.myMaximumBy (\_ _ -> LT) [1..10] `shouldBe` 10

    it "should return the higher when using compare" $ do  
      SUT.myMaximumBy compare [1..10] `shouldBe` 10

  describe "myMinimumBy" $ do
    it "should return the last when all is 'great'" $ do  
      SUT.myMinimumBy (\_ _ -> GT) [1..10] `shouldBe` 10
    
    it "should return the head when more is 'less'" $ do  
      SUT.myMinimumBy (\_ _ -> LT) [1..10] `shouldBe` 1

    it "should return the lower when using compare" $ do  
      SUT.myMinimumBy compare [1..10] `shouldBe` 1      