myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (||) False . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (||) False . map ((==) x)

myElemWithAny :: Eq a => a -> [a] -> Bool
myElemWithAny x = any ((==) x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) [] 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> (f a) : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> (f a) ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\a b -> if f a b == LT then a else b) x xs