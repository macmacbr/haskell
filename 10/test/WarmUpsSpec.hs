module WarmUpsSpec where

import qualified WarmUps as SUT

import Test.Hspec

spec :: Spec
spec = do
  describe "createSVS" $ do
    it "should create a bunch of combinations, including 'keg'" $ do
      SUT.createSVS `shouldContain` ["keg"]

  describe "onlyPVS" $ do
    it "should have a list begining with 'p'" $ do
      SUT.onlyPSVS `shouldContain` ["pig"]

  describe "onlyPVS" $ do
    it "should have a list begining with 'p'" $ do
      SUT.onlyPSVS `shouldNotContain` ["keg"]
      
  describe "createNVN" $ do
    it "should create list of Nouns Verbs Nouns" $ do
      SUT.createNVN `shouldContain` [("pin","poked","pineapple")]