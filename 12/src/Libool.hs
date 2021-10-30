module Libool where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee z _ Nothing = z 

fromMaybe :: a -> Maybe a -> a
fromMaybe z = mayybee z id
  
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : _) = Just a

maybeToList :: Maybe a -> [a]
maybeToList = mayybee ([] :: [a]) (:[])

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

maybeToMaybeList :: [a] -> Maybe a -> Maybe [a]
maybeToMaybeList as = mayybee Nothing (Just . (:as))

cantBe :: Maybe a ->  Maybe [a] -> Maybe [a]
cantBe _ Nothing = Nothing
cantBe mb (Just as) = maybeToMaybeList as mb

-- sequence that nukes if there's any Nothing in it.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr cantBe (Just [])
       
       
--   EITHER
type EIS = Either Int String  --for the tests

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
   where 
     f (Right _) as = as
     f (Left a) as = a : as
     
rights' :: [Either a b] -> [b]
rights' = foldr f []
   where
     f (Right a) as = a : as
     f (Left _) as = as
     
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
   where
     f (Right b) (as, bs) = (as, b: bs)
     f (Left a) (as, bs) = (a: as, bs)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' fa _ (Left a) = fa a
either' _ fb (Right b) = fb b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' fb = either' (const Nothing) (Just . fb)

