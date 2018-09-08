module Possibly where

import Test.QuickCheck

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    elements [LolNope, (Yeppers a)]