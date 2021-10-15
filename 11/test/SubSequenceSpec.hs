module SubSequenceSpec where
  
import qualified SubSequence as SUT
import Test.Hspec

spec :: Spec
spec = do
  describe "SubSequence" $ do
    it "should find a single char as subsequence" $ do
      SUT.isSubseqOf "a" "abc" `shouldBe` True
      
    it "should not find a single char as subsequence" $ do
      SUT.isSubseqOf "e" "abc" `shouldBe` False  

    it "should find a single char as subsequence anywhere" $ do
      SUT.isSubseqOf "a" "xxx babc" `shouldBe` True    

    it "should find a word as subsequence" $ do
      SUT.isSubseqOf "abcd" "xxxx abcd efg" `shouldBe` True     
    
    it "should not find a word as subsequence" $ do
      SUT.isSubseqOf "abcd" "xxxx yyyy efg" `shouldBe` False    
      
    it "should find a word as subsequence sparse" $ do
      SUT.isSubseqOf "abcd" "xxaxb xxcxxxd xx" `shouldBe` True  
      
    it "should find a word as subsequence sparse with repetition" $ do
      SUT.isSubseqOf "abcd" "xxaxb xxcxc  cxd xx" `shouldBe` True    

    it "should not find a word as subsequence sparse if not complete" $ do
      SUT.isSubseqOf "abcd" "xxaxb xxcx xxx xx" `shouldBe` False  

    it "should not find a word as subsequence sparse if not in order" $ do
      SUT.isSubseqOf "ab" "ba" `shouldBe` False  