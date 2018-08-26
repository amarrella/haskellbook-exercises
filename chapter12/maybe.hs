isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing y = not (isJust y)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b 
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe b Nothing = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x):xs) = x : (catMaybes xs)
catMaybes (Nothing:xs) = catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs =
  if (any isNothing xs) then Nothing
  else Just (catMaybes xs)