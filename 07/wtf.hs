--- wtf.hs
module Wtf where

wtf :: (a, b) -> (c, d) -> ((b, d), (a, c))
wtf (a, b) (c, d) = ((b, d), (a, c))
