lefts' :: [Either a b] -> [a]
lefts' xs =
  foldr (\a b -> leftToList a ++ b) [] xs
    where leftToList (Left x) = [x]
          leftToList _ = []

rights' :: [Either a b] -> [b]
rights' xs =
  foldr (\a b -> rightToList a ++ b) [] xs
    where rightToList (Right x) = [x]
          rightToList _ = []

partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' xs =
  (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c)
            -> Either a b
            -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right a) = 
  Just (f a)

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a) =
  f a
either' _ g (Right b) =
  g b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f x =
  either' (\_ -> Nothing) (\y -> Just (f y)) x
