module MonoidExercises where
import Test.QuickCheck
import Data.Semigroup
import SemigroupExercises

-- 1
instance Monoid Trivial where 
  mempty = Trivial
  mappend = (<>)

-- 2
instance (Monoid a) => Monoid (Identity a) where 
  mempty = Identity mempty
  mappend = (<>)

-- 3
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

-- 4
instance Monoid BoolConj where
  mempty = BoolConj False
  mappend = (<>)

-- 4
instance Monoid BoolDisj where
  mempty = BoolDisj True
  mappend = (<>)

-- 6
--instance (Monoid a, Monoid b) => Monoid (Combine a b) where
--  mempty = Combine (\a -> a)
--  mappend = (<>)

-- 7
--instance (Monoid a) => Monoid (Comp a) where
--  mempty = Comp (\a -> a)
--  mappend = (<>)

-- 8

newtype Mem s a = 
  Mem {runMem :: s -> (a,s) }

instance (Semigroup a) => Semigroup (Mem s a) where 
  Mem f <> Mem g = 
    Mem $ \s -> 
      let (a,s') = g s in
        let (a',s'') = f s' in
          (a <> a', s'')

instance Monoid a => Monoid (Mem s a) where 
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

type MemAssoc s a =
  (Mem s a) -> (Mem s a) -> (Mem s a) -> Bool