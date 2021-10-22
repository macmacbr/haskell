module PhoneTextSpec where

import qualified PhoneText as SUT
import Test.Hspec

spec :: Spec
spec = do
  describe "SubSequence" $ do
    it "should find a key on a phone" $ do
      SUT.findTextKey SUT.myPhone 'A' `shouldBe` Just SUT.key2
      
  describe "Key to Sequence" $ do
    it "should get the tapping sequence for a Char C" $ do
      SUT.reverseTaps SUT.myPhone 'C' `shouldBe` [('*', 1), ('2', 3)]   {-- key "*" for uppercase, key "2" pressed 3 times for 'C' -}
    
    it "should get the tapping sequence for a Char c lowercase" $ do
      SUT.reverseTaps SUT.myPhone 'c' `shouldBe` [('2', 3)]   {-- key "2" pressed 3 times for 'c' -}
    
    it "should get the tapping sequence for a Char 2" $ do
      SUT.reverseTaps SUT.myPhone '2' `shouldBe` [('2', 4)]   {-- key "2" pressed 3 times for '2' -}
  
  describe "Text to Sequence" $ do
    it "should show no tappings for no text" $ do
      SUT.cellPhonesDead SUT.myPhone "" `shouldBe` []

    it "should show the tappings necessary for a text" $ do
      SUT.cellPhonesDead SUT.myPhone "ab" `shouldBe` [('2',1),('2',2)]
          
    it "should show the tappings necessary for a text with space" $ do
      SUT.cellPhonesDead SUT.myPhone "ab Cd" `shouldBe` [('2',1),('2',2),('0',2),('*',1),('2',3),('3',1)]
      
    it "should show the tappings necessary for Marco" $ do
      SUT.cellPhonesDead SUT.myPhone "Marco" `shouldBe` [('*',1),('6',1),('2',1),('7',3),('2',3),('6',3)]
  
  describe "Number of taps" $ do
    it "should count the total number of taps" $ do
      SUT.fingerTaps [('*',1),('6',1),('2',1),('7',3),('2',3),('6',3)] `shouldBe` 12
    
    it "should count the total number of taps for no taps" $ do
      SUT.fingerTaps [] `shouldBe` 0    
  
  describe "most Popular Letter" $ do
    it "should get the mode letter" $ do
      SUT.mostPopularLetter "ab cde cc d" `shouldBe` Just 'C'