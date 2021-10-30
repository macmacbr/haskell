module LiboolSpec where

import qualified Libool as SUT

import Test.Hspec

spec :: Spec

spec = do
  describe "bool" $ do
    it "should assert filled maybe" $ do
      SUT.isJust (Just 1) `shouldBe` True
      SUT.isJust (Just "a") `shouldBe` True
      SUT.isJust Nothing `shouldBe` False

    it "should assert emptyness" $ do
      SUT.isNothing (Just 1) `shouldBe` False
      SUT.isNothing (Just "a") `shouldBe` False
      SUT.isNothing Nothing `shouldBe` True

  describe "mayybee" $ do
    it "should apply a method only on filled Maybe" $ do
      SUT.mayybee "you're it" ("not " ++) (Just "it") `shouldBe` "not it"
      SUT.mayybee "you're it" ("not " ++) Nothing `shouldBe` "you're it"

    it "should fallback" $ do
      SUT.fromMaybe 0 Nothing `shouldBe` 0
      SUT.fromMaybe 0 (Just 1) `shouldBe` 1

    it "should create a list" $ do
      SUT.maybeToList (Just 1) `shouldBe` [1]
      SUT.maybeToList Nothing `shouldBe` ([] :: [Int])

    it "should maybe a list" $ do
      SUT.listToMaybe [1,2] `shouldBe` Just 1
      SUT.listToMaybe ([] :: [Int]) `shouldBe` Nothing

    it "should flatten a list" $ do
      SUT.catMaybes [Just 1, Nothing, Just 2] `shouldBe` [1, 2]
      SUT.catMaybes (replicate 3 (Nothing :: Maybe Int)) `shouldBe` []

    it "should sequence" $ do
      SUT.flipMaybe [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3]
      SUT.flipMaybe [Just 1, Nothing, Just 3] `shouldBe` Nothing
      SUT.flipMaybe [] `shouldBe` Just ([] :: [Int])
  
  describe "eithers" $ do

    it "should do lefts" $ do
      SUT.lefts' [Left 1, Left 2] `shouldBe` [1, 2]
      SUT.lefts' [Left 1, Right "a"] `shouldBe` [1]
      SUT.lefts' ([] :: [SUT.EIS]) `shouldBe` []
      
    it "should do rights" $ do
      SUT.rights' [Right "a", Right "b"] `shouldBe` ["a", "b"]
      SUT.rights' [Right "a", Left 1] `shouldBe` ["a"]
      SUT.rights' ([] :: [SUT.EIS]) `shouldBe` []
    
    it "should partition" $ do
      SUT.partitionEithers' [Right "a", Left 1, Right "b"] `shouldBe` ([1], ["a", "b"])
      SUT.partitionEithers' ([] :: [SUT.EIS]) `shouldBe` ([], [])
      
    it "should get a right value" $ do
      SUT.eitherMaybe' (+1) (Right 2) `shouldBe` Just 3
      SUT.eitherMaybe' (+1) (Left 2) `shouldBe` Nothing
    
    
    
    it "should get a right value applied" $ do
      SUT.eitherMaybe'' (+1) (Right 2) `shouldBe` Just 3
      SUT.eitherMaybe'' (+1) (Left 2) `shouldBe` Nothing      