module Naturals where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i = case compare i 0 of
      LT -> Nothing
      EQ -> Just Zero
      GT -> case integerToNat (i - 1) of
          Nothing -> Nothing
          Just n -> Just (Succ n)
          
