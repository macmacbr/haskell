module TodoSpec where

import qualified Lib as SUT
import Test.Hspec

spec :: Spec

spec = do
  describe "loaded" $ do
     it "should load the minimum test framework" $ do
       SUT.loaded `shouldBe` True