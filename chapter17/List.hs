module List where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons a as)
  | n <= 0    = Nil
  | otherwise = Cons a $ take' (n-1) as

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) =
    Cons (f a) (fmap f as)

append :: List a -> List a -> List a 
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = 
  f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b 
flatMap f as = 
  concat' $ fmap f as

instance Applicative List where 
  pure a      = Cons a Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  fs <*> xs = 
    flatMap (`fmap` xs) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a   <- arbitrary
    as  <- arbitrary
    elements [Nil, Cons a as]

instance Eq a => EqProp (List a) where
  (=-=) = eq

l = Cons ("hello", "hi", "howdy") Nil

newtype ZipList' a = 
  ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where 
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs 
                in take' 3000 l
          ys' = let (ZipList' l) = ys 
                in take' 3000 l
  
instance Functor ZipList' where 
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs
  
instance Applicative ZipList' where 
  pure = ZipList' . pure 
  (ZipList' fs) <*> (ZipList' xs) = 
    ZipList' $ flatMap (`fmap` xs) fs

instance Arbitrary as => Arbitrary (ZipList' as) where
  arbitrary = do
    as <- arbitrary
    return $ ZipList' as
    
main = do
  quickBatch $ applicative l
  quickBatch $ applicative (ZipList' l)