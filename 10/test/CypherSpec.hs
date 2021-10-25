module CypherSpec where

import qualified Cypher as SUT
import Test.Hspec

spec :: Spec
spec = do
  describe "rotateChar" $ do
    it "should rotate a single Char" $
      SUT.rotateChar 1 'a' `shouldBe` 'b'

  describe "ceaser" $ do
    it "should make a rot1 of a test" $
      SUT.caesar 1 "abc def" `shouldBe` "bcd efg"

  describe "rotateCharKey" $ do
    it "should rotate the Char and the charkey" $ do
       SUT.rotateCharKey 'a' (SUT.RotCharRes "" (SUT.CharKey"abc")) `shouldBe`
                              SUT.RotCharRes "a" (SUT.CharKey "bca")
       SUT.rotateCharKey 'd' (SUT.RotCharRes "abc" (SUT.CharKey"bcd")) `shouldBe`
                              SUT.RotCharRes "abce" (SUT.CharKey "cdb")

    it "should rotate none" $ do
       SUT.rotateCharKey ' ' (SUT.RotCharRes "" (SUT.CharKey"abc")) `shouldBe`
                              SUT.RotCharRes " " (SUT.CharKey "abc")
       SUT.rotateCharKey '%' (SUT.RotCharRes "abc" (SUT.CharKey"bcd")) `shouldBe`
                              SUT.RotCharRes "abc%" (SUT.CharKey "bcd")

    it "should not rotate with key A" $ do
       SUT.rotateCharKey 'M' (SUT.RotCharRes "" (SUT.CharKey "A")) `shouldBe`
                              SUT.RotCharRes "M" (SUT.CharKey "A")