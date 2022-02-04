module ListyInstances where

import Data.Monoid
import Listy


{-   
  This code is a demo of where to not put instances. This will give an error from the compiler.
  If there's any need to decorate a type, create a "newtype"  ~ AnyVal

instance Semigroup (Listy a) where
    (<>) (Listy l) (Listy l') =
        Listy $ mappend l l'

instance Monoid (Listy a) where
    mempty = Listy []

-}