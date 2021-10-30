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
           