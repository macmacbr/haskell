module DataBaseSpec where

import qualified DataBase as SUT
import Test.Hspec
import Data.Time

spec :: Spec
spec = do
  describe "filterDbDate" $ do
    it "get a list of Dates from a DB" $ 
       SUT.filterDbDate SUT.theDatabase `shouldNotBe` []
  
  describe "filterDbNumber" $ do
    it "get a list of Numbers from the DB" $
       SUT.filterDbNumber SUT.theDatabase `shouldNotBe` []
    
  describe "mostRecent"  $ do
    it "should get the most recent date from the DB" $
      SUT.mostRecent SUT.theDatabase `shouldBe` 
          UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
  
  describe "sumNumbers" $ do
    it "should sum all numbers from the DB" $
      SUT.sumDb SUT.theDatabase `shouldBe` 9001+8001
      
  describe "avgDB1" $ do
    it "should calculate the average of the DB" $
      SUT.avgDb1 SUT.theDatabase `shouldBe` (9001+8001)/2
      
  describe "avgDB" $ do
    it "should calculate the average of the DB" $
      SUT.avgDb SUT.theDatabase `shouldBe` (9001+8001)/2