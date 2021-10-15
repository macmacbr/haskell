module PhoneTextSpec where

import qualified PhoneText as SUT
import Test.Hspec

spec :: Spec
spec = do
  describe "SubSequence" $ do
    it "should find a key on a phone" $ do
      SUT.findTextKey SUT.myPhone 'A' `shouldBe` Just SUT.key2