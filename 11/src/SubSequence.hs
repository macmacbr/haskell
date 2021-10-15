module SubSequence where

-- 449.1 if a Text contains all letters from the Seq in that order.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf key text = aux (key, text) 
   where 
     aux ([], _) = True
     aux (k:_, []) = False 
     aux (kf@(k:ks), t:ts) = if k == t then aux (ks, ts) else aux (kf, ts)
