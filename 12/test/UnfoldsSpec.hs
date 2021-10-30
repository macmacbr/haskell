module UnfoldsSpec where

import qualified Unfolds as SUT

import Unfolds (BinaryTree(..))

import Test.Hspec
spec :: Spec

spec = do
  describe "unfolds" $ do
    it "should have a test" $ do
      take 10 (SUT.myIterate (+1) 0) `shouldBe` [0,1,2,3,4,5,6,7,8,9]
     
    it "should unfold a list" $ do
      take 10 (SUT.myUnfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10) `shouldBe` reverse [1..10]
      
    it "should use unfolder to build iterate" $ do
      take 10 (SUT.betterIterate (+1) 0) `shouldBe` [0,1,2,3,4,5,6,7,8,9]
      
    it "should build a tree" $ do
      SUT.treeBuild 2 `shouldBe` 
      (Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)))

