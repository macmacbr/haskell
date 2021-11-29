module PropertySpec where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "property" $ do
    it "should be 2" $ do
      1 + 1 `shouldBe` 2
    
    prop "+1 is always more" $  \x -> (x + 1) > (x :: Int)
 