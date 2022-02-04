module MonadSpec where

import Monoid (Optional(..), Mem(..))

import Test.Hspec

import Data.Monoid

spec :: Spec

type MAT = String -> String -> String -> Bool

spec = do
  describe "monoids" $ do
    let onlySum = Only (Sum 1)

    it "should combine" $
       (onlySum `mappend` onlySum) `shouldBe` Only (Sum {getSum = 2})

    it "should combine with Nada" $
       (onlySum `mappend` Nada) `shouldBe` Only (Sum {getSum = 1})

    it "should be combined with Nada" $
       (Nada `mappend` onlySum) `shouldBe` Only (Sum {getSum = 1})

    it "should combine products" $
      (Only (Product 2) `mappend` Only (Product 3)) `shouldBe` Only (Product {getProduct = 6})

    it "should combine arrays" $ do
       Only [1] `mappend` Nada `shouldBe` Only [1]
       Only [1] `mappend` Only [2] `shouldBe` Only [1, 2]

--  describe "assiciativity" $ do
--    prop "should check associativity on the monoid" $
--       (\s1 s2 s3 -> (SUT.monoidAssoc :: MAT)) `shouldBe` True
  describe "Mem" $ do
    let f' = Mem $ \s -> ("hi", s + 1)
    let rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0

    it "should eveluate zero" $
      (runMem mempty 0 :: (String, Int)) `shouldBe` ("", 0)

    it "should evaluate left" $
      rmleft `shouldBe` ("hi", 1)

    it "should evaluate right" $
      rmright `shouldBe` ("hi", 1)