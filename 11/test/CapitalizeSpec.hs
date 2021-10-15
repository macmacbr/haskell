module CapitalizeSpec where

import qualified Capitalize as SUT
import Test.Hspec

spec :: Spec
spec = do
  describe "Capitalize" $ do
    it "should capitalize first letter only" $ do
      SUT.capitalizeWord "hi there" `shouldBe` "Hi there"
      
  describe "SplitTupple" $ do
    it "should return a sinle word" $ do
       SUT.splitator SUT.dropper ' ' "hi" `shouldBe` ["hi"]
    
    it "should split a text by spaces" $ do
      SUT.splitator SUT.dropper ' ' "hi there you" `shouldBe` ["hi", "there", "you"]

    it "should split text in tuples and capitalize" $ do
      SUT.capitalizeWords "hi there you" `shouldBe` [("hi", "Hi"), ("there", "There"), ("you", "You")]
  
  describe "CapitalizeParagraph" $ do
    it "should capitalize whole phares in a paragraph" $ do
      SUT.capitalizeParagraph  "hi there you.  busy aren't you. time for a beer." 
                    `shouldBe` "Hi there you.  Busy aren't you. Time for a beer."