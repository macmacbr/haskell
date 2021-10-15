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
    