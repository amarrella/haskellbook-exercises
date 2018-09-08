{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where

import Test.QuickCheck
import GHC.Arr

-- 1
data Bool' =
  False | True
-- Can't create a functor

-- 2
data BoolAndSomethingElse a =
  False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    elements [(False' a), (True' a)]

-- 3
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

instance (Arbitrary a) => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    elements [Falsish, (Truish a)]
  
-- 4
newtype Mu f = InF { outF :: f (Mu f) }
-- can't because f is of kind * -> *

-- 5
data D = D (Array Word Word) Int Int
-- can't because D is of kind *

--------------------
---- SECOND BATCH
--------------------
-- 1
data Sum b a = 
    First a
  | Second b

instance Functor (Sum e) where 
  fmap f (First a) = First (f a) 
  fmap f (Second b) = Second b

-- 2
data Company a c b = 
    DeepBlue a c
  | Something b

instance Functor (Company e e') where 
  fmap f (Something b) = Something (f b) 
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More b a = 
  L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a') 
  fmap f (R b a b') = R b (f a) b'

--------------------
---- THIRD BATCH
--------------------
-- 1
data Quant a b = 
  Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3

newtype Flip f a b = 
  Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' (f a)

-- 4
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
  
-- 5
data LiftItOut f a =
  LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7 
data IgnoreOne f g a b = 
  IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8
data Notorious g o a t = 
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9
data List a = 
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = 
    Cons (f a) (fmap f la)

-- 10
data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats fa ga ha) =
    MoreGoats (fmap f fa) (fmap f ga) (fmap f ha)

-- 11

data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sToa) = Read (fmap f sToa)