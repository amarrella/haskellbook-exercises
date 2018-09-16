import Data.Monoid
import Data.Semigroup

-- 1
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

-- 2
product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

-- 3
elem :: (Foldable t, Eq a)
     => a -> t a -> Bool
elem a = getAny . foldMap (\x -> Any(x==a) )

-- 4
minimum :: (Foldable t, Ord a, Bounded a)
        => t a -> Maybe a
minimum xs
  | Main.null xs   = Nothing
  | otherwise = Just $ getMin $ foldMap Min xs

-- 5
maximum :: (Foldable t, Ord a, Bounded a)
        => t a -> Maybe a
maximum xs
  | Main.null xs   = Nothing
  | otherwise = Just $ getMax $ foldMap Max xs

-- 6
null :: (Foldable t) => t a -> Bool
null = getAll . foldMap (\_ -> All False)

-- 7
length :: (Foldable t) => t a -> Int
length = getSum . foldMap (\_ -> Sum 1)

-- 8
toList :: (Foldable t) => t a -> [a]
toList = foldMap (:[])

-- 9
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10
foldMap' :: (Foldable t, Monoid m)
        => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty 