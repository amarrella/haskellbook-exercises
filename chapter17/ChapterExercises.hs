module ChapterExercises where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair a b) = 
    Pair (f a) (g b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do 
    a <- arbitrary
    Pair a <$> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

pair = Pair ("a", "b", "c")  ("d", "e", "f")

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a f) <*> (Two a' b) = 
    Two (a<>a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do 
    a <- arbitrary
    Two a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

two = Two [1::Int] ("d", "e", "f")

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' c) = 
    Three (a<>a') (b<>b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

three = Three [1::Int] "a" ("d", "e", "f")

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a f g) <*> (Three' a' b c) = 
    Three' (a<>a') (f b) (g c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    Three' a b <$> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

three' = Three' [1::Int] ("a", "b", "c") ("d", "e", "f")

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a b c f) <*> (Four a' b' c' d) = 
    Four (a<>a') (b<>b') (c<>c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

four = Four [1::Int] [1::Int] [1::Int] ("d", "e", "f")

-- 6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a b c f) <*> (Four' a' b' c' d) = 
    Four' (a<>a') (b<>b') (c<>c') (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four' a b c <$> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

four' = Four' [1::Int] [1::Int] [1::Int] ("d", "e", "f")

-- Combinations
stops :: String
stops = "pbtdkg" 

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

main = do
  quickBatch $ applicative pair
  quickBatch $ applicative two
  quickBatch $ applicative three
  quickBatch $ applicative three'
  quickBatch $ applicative four
  quickBatch $ applicative four'