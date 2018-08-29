module QuickCheckTests where

import Test.QuickCheck
import Data.List (sort)

half x = x / 2
halfIdentity = (*2) . half

prop_half :: Double -> Bool
prop_half x = half x == half (halfIdentity x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs = listOrdered (sort xs)

plusAssociative :: (Integral a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Integral a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

productAssociative :: (Integral a) => a -> a -> a -> Bool
productAssociative x y z = x * (y * z) == (x * y) * z

productCommutative :: (Integral a) => a -> a -> Bool
productCommutative x y = x * y == y * x

prop_quotrem :: (Integral a) => a -> a -> Bool
prop_quotrem x y 
  | y == 0 = True -- Avoiding division by zero
  | otherwise = (quot x y)*y + (rem x y) == x

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary 
  b <- arbitrary 
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c) 
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary 
  return (a, b, c)

differenceNotAssociative :: Property
differenceNotAssociative = 
  expectFailure $ forAll (genThreeple :: Gen (Integer, Integer, Integer)) $ \(x, y, z) -> x - (y - z) == (x - y) - z

differenceNotCommutative :: Property
differenceNotCommutative = 
  expectFailure $ forAll (genTuple :: Gen (Integer, Integer)) $ \(x, y) -> x - y == y - x
  
reverseIdentity :: (Eq a) => [a] -> Bool
reverseIdentity xs = reverse(reverse xs) == id xs

dollarSign_prop :: (Eq b) => (a -> b) -> a -> Bool
dollarSign_prop f a = (f $ a) == f a

dotComposition_prop :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
dotComposition_prop f g a = ((f . g) a) == ((\x -> f (g x)) a)

foldrConsEqual :: (Eq a) => [a] -> [a] -> Bool
foldrConsEqual b xs = foldr (:) b xs == (++) xs b

foldrConcat :: (Eq a) => [[a]] -> Bool
foldrConcat xs = (foldr (++) [] xs) == (concat xs)

lengthTakeN :: Property
lengthTakeN = 
  expectFailure $ forAll genNList $ \(n, xs) -> length (take n xs) == n
    where genNList :: Gen (Int, [Integer])
          genNList = do
            a <- arbitrary
            b <- listOf arbitrary
            return (a, b)

readShow :: (Eq a, Read a, Show a) => a -> Bool
readShow x =
  (read (show x)) == x

runQc :: IO ()
runQc = do 
  quickCheck prop_half
  quickCheck (prop_listOrdered :: [Int] -> Bool) 
  quickCheck (plusAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (plusCommutative :: Integer -> Integer -> Bool)
  quickCheck (productAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (productCommutative :: Integer -> Integer -> Bool)
  quickCheck (prop_quotrem :: Integer -> Integer -> Bool)
  quickCheck differenceNotAssociative
  quickCheck differenceNotCommutative
  quickCheck (reverseIdentity :: [Integer] -> Bool)
  quickCheck (dollarSign_prop (+1) :: Integer -> Bool)
  quickCheck (dotComposition_prop even (+1) :: Integer -> Bool) 
  quickCheck (foldrConsEqual :: [Integer] -> [Integer] -> Bool)
  quickCheck (foldrConcat :: [[Integer]] -> Bool)
  quickCheck lengthTakeN
  quickCheck (readShow :: Integer -> Bool)