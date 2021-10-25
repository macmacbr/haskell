module HuttonRazorSpec where

import qualified HuttonRazor as SUT

import Test.Hspec
import HuttonRazor (Expr(..))


spec :: Spec
spec = do
  describe "eval" $ do
    it "should evaluate a number" $
      SUT.eval (Lit 1) `shouldBe` 1
    
    it "should evaluate a sum" $
      SUT.eval (Add (Lit 1) (Lit 2)) `shouldBe` 3

  describe "printExpr" $ do
    it "should print a simple expression" $
      SUT.printExpr (Lit 2) `shouldBe` "2"
    
    it "should print a simple sum" $
      SUT.printExpr (Add (Lit 1) (Lit 2)) `shouldBe` "1 + 2"
     
    it "should print nested expressions" $ do
      let a1 = Add (Lit 9001) (Lit 1)
      let a2 = Add a1 (Lit 20001)
      let a3 = Add (Lit 1) a2
      SUT.printExpr a3 `shouldBe` "1 + 9001 + 1 + 20001"
  