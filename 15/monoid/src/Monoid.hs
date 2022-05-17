module Monoid where

--chapter 15  

import Data.Monoid

data Booly a =
           False'
         | True'
    deriving (Eq, Show)

instance Semigroup (Booly a) where
  -- (<>) :: (Booly a) -> (Booly a) -> (Booly a)
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) True' True' = True'

instance Monoid (Booly a) where
  mempty = True'

-- Optional Monoid
data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada other = other
  (<>) other Nada = other
  (<>) (Only x) (Only y) = Only ((<>) x y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)


-- MadLib

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."


-- with mconcat
madlibbinBetter' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbinBetter' e adv noun adj =
    mconcat [e, "! he said ",
             adv, " as he jumped into his car ",
             noun, " and drove off with his ",
             adj, " wife."]

-- test monoid associativity
monoidAssoc :: (Eq m, Monoid m)
  => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)


-- exercise 8
newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) rm1 rm2 = Mem ct
    where
       ct sp =
         let t1 = runMem rm1 sp
             t2 = runMem rm2 (snd t1) in
               (fst t1 <> fst t2, snd t2)

instance Monoid a => Monoid (Mem s a) where
  mempty  = Mem (\ss -> (mempty, ss))




--- let f' = Mem $ \s -> ("hi", s + 1)
---    let -- rmzero = runMem mempty 0
--        rmleft = runMem (f' <> mempty) 0
--        rmright = runMem (mempty <> f') 0


--    it "should evaluate left" $
--      rmleft `shouldBe` ("hi", 1)
