module SemigroupExercises where

import Test.QuickCheck(Arbitrary, CoArbitrary, arbitrary, elements)
import Data.Semigroup
import Laws
-- 1
data Trivial = Trivial deriving (Eq, Show) 

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

type IdentityAssoc a =
  Identity a -> Identity a -> Identity a -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- 3
data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

type TwoAssoc a b =
  (Two a b) -> (Two a b) -> (Two a b) -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- 4
data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

type ThreeAssoc a b c =
  (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four x1 y1 z1 a1) <> (Four x2 y2 z2 a2) = Four (x1 <> x2) (y1 <> y2) (z1 <> z2) (a1 <> a2)

type FourAssoc a b c d =
  (Four a b c d) -> (Four a b c d) -> (Four a b c d) -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

-- 6
newtype BoolConj =
  BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj False <> BoolConj False = BoolConj False
  _              <> _              = BoolConj True

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return (BoolConj a)

-- 7 
newtype BoolDisj =
  BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True  <> BoolDisj True   = BoolDisj True
  _              <> _               = BoolDisj False

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return (BoolDisj a)

-- 8 
data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where 
  (Fst x) <> (Fst y) = Fst y
  (Fst x) <> (Snd y) = Snd y
  (Snd x) <> (Fst y) = Snd x
  (Snd x) <> (Snd y) = Snd x 

type OrAssoc a b =
 (Or a b) -> (Or a b) -> (Or a b) -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fst a), (Snd b)]

-- 9
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where 
  (Combine f) <> (Combine g) = 
    Combine $ \a -> f a <> g a

type CombineAssoc a b =
  (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool

-- instance (Arbitrary a) => Arbitrary (Combine a (Sum(a))) where 
--  arbitrary = return $ Combine Sum
-- (╯°□°）╯︵ ┻━┻

-- 10 
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where 
  (Comp f) <> (Comp g) = Comp (f . g)

type CompAssoc a =
  (Comp a) -> (Comp a) -> (Comp a) -> Bool

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    a <- arbitrary
    return (Comp a)

-- TODO finish 9 and 10

data Validation a b = 
  Failure a | Success b 
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success a) <> _ = Success a
  (Failure a) <> (Failure b) = Failure $ a <> b
  _ <> (Success b) = Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Failure a), (Success b)]

type ValidationAssoc a b =
  (Validation a b) -> (Validation a b) -> (Validation a b) -> Bool
