module Chapter12Spec where

import qualified Chapter12 as SUT
import Test.Hspec

spec :: Spec

spec = do
  describe "mkpersonOne" $ do
     it "should create a person" $ do
       SUT.mkPersonOne "John" 13 `shouldBe` Just (SUT.Person "John" 13)

     it "should not create broken person" $ do
       SUT.mkPersonOne "" 160 `shouldBe` Nothing
       SUT.mkPersonOne "Anci Joe" (-10) `shouldBe` Nothing

  describe "mkpersonTwo" $ do
     it "should create a person" $ do
       SUT.mkPersonTwo "John" 13 `shouldBe` Right (SUT.Person "John" 13)

     it "should not create broken person" $ do
       SUT.mkPersonTwo "" 160 `shouldBe` Left SUT.NameEmpty
       SUT.mkPersonTwo "Anci Joe" (-10) `shouldBe` Left SUT.AgeTooLow

  describe "mkperson" $ do
     it "should create a person" $ do
       SUT.mkPerson "John" 13 `shouldBe` Right (SUT.Person "John" 13)

     it "should not create broken person" $ do
       SUT.mkPerson "" 160 `shouldBe` Left [SUT.NameEmpty]
       SUT.mkPerson "Anci Joe" (-10) `shouldBe` Left [SUT.AgeTooLow]
       SUT.mkPerson "" (-10) `shouldBe` Left [SUT.NameEmpty, SUT.AgeTooLow]
       
  describe "stringProcessing" $ do
    describe "notThe" $ do
      it "should suppress the word `the`" $ do
        SUT.notThe "the" `shouldBe` Nothing
        SUT.notThe "The" `shouldBe` Nothing
        SUT.notThe "has The" `shouldBe` Just "has The"
        SUT.notThe "what" `shouldBe` Just "what"
    
    describe "strTokenizer" $ do
      it "should tokenize a text" $ do
        SUT.strTokenizer "The not-the-true-colored bird should, if pleased - note: it is; be ♥love. So? 33 End" `shouldBe` 
          ["The"," ",
           "not","-","the","-","true","-","colored",   -- I whish this could be a single word
           " ","bird"," ","should",",",
           " ","if"," ","pleased"," ","-"," ","note",":"," ","it"," ","is",";",
           " ","be"," ","\9829love","."," ",
           "So","?"," ","33"," ","End"]
      
      it "should not blow with empty text" $ do
        SUT.strTokenizer "" `shouldBe` [""]
    
    describe "replaceThe" $ do
      it "should replace occurences of 'the' by 'a'" $ do
        SUT.replaceThe "The not-the-true-colored bird should, if pleased - note: it is; be ♥love. So? 33 End" `shouldBe`
            "a not-a-true-colored bird should, if pleased - note: it is; be \9829love. So? 33 End"
    
    describe "countTheBeforeVowel" $ do
      it "should tell if first letter is vowel" $ do
        SUT.firstLetterIsVowel "abracadabra" `shouldBe` True
        SUT.firstLetterIsVowel "banana" `shouldBe` False
        SUT.firstLetterIsVowel "" `shouldBe` False
         
      it "should find number of 'the' before a word that starts with a vowel" $ do
        SUT.countTheBeforeVowel "The albatroz and the fox went to the theater in the afternoon." `shouldBe` 2
        
    describe "countVowels" $ do
      it "should count vowels only" $ do
        SUT.countVowels "my dear text? is this valid for you" `shouldBe` 10
        SUT.countVowelsTheHardWay "my dear text? is this valid for you" `shouldBe` 10
    
    describe "invalidWord" $ do
      it "invalidates the word if there are too many vowels" $ do
        SUT.validWord "xxxaa" `shouldBe` Just (SUT.Word' "xxxaa")
        SUT.validWord "xxxaaaa" `shouldBe` Nothing