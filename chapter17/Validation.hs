module Validation where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data Validation e a = 
    Failure e
  | Success a deriving (Eq, Show)

instance Functor (Validation e) where 
  fmap _ (Validation.Failure e) = Validation.Failure e
  fmap f (Validation.Success a) = Validation.Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure            = Validation.Success
  (Validation.Success f) <*> (Validation.Success a) 
    = Validation.Success (f a)
  (Validation.Success f) <*> (Validation.Failure e) 
    = Validation.Failure e 
  (Validation.Failure e) <*> (Validation.Success a) 
    = Validation.Failure e 
  (Validation.Failure e) <*> (Validation.Failure e') 
    = Validation.Failure (e <> e')

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where 
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Validation.Failure e, Validation.Success a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

v = (Validation.Success (1, 2, 3)) :: Validation String (Int, Int, Int)

main = quickBatch $ applicative v