{-# LANGUAGE FlexibleContexts #-}
module ChapterExercises where 

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  sequenceA (Identity fa) = 
    Identity <$> fa

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- 2

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
  sequenceA (Constant a) =
    Constant <$> pure a

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

-- 3
data Optional a =
    Nada
  | Yep a deriving (Eq, Show, Ord)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep fa) = Yep <$> fa
   
instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do 
    a <- arbitrary
    elements [Nada, Yep a]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq
  
-- 4
data List a =
    Nil
  | Cons a (List a) deriving (Eq, Show, Ord)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = 
    Cons (f a) (fmap f as)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) =
    f a <> foldMap f as

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons fa fas) =
    Cons <$> fa <*> sequenceA fas

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do 
    a <- arbitrary
    as <- arbitrary
    elements [Nil, Cons a as]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

-- 5
data Three a b c =
  Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) =
    f c

instance Traversable (Three a b) where
  sequenceA (Three a b fc) =
    Three a b <$> fc

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 6
data Pair a b =
  Pair a b deriving (Eq, Show, Ord)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) =
    f b

instance Traversable (Pair a) where
  sequenceA (Pair a fb) =
    Pair a <$> fb

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do 
    a <- arbitrary
    Pair a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- 7
data Big a b =
  Big a b b deriving (Eq, Show, Ord)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') =
    f b <> f b'

instance Traversable (Big a) where
  sequenceA (Big a fb fb') =
    Big a <$> fb <*> fb'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do 
    a <- arbitrary
    Big a <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- 8
data Bigger a b =
  Bigger a b b b deriving (Eq, Show, Ord)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') =
    f b <> f b' <> f b''

instance Traversable (Bigger a) where
  sequenceA (Bigger a fb fb' fb'') =
    Bigger a <$> fb <*> fb' <*> fb''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do 
    a <- arbitrary
    Bigger a <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- 9 
data S n a = 
  S (n a) a deriving (Eq, Show)

instance ( Functor n , Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where 
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = 
    S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = 
    foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = 
    S <$> traverse f na <*> f a

-- 10
data Tree a = 
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a) 
  deriving (Eq, Show)

instance Functor Tree where 
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = 
    Node (fmap f l) (f a) (fmap f r) 

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = 
    foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = 
    Node <$> traverse f l <*> f a <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    elements [Empty, Leaf a, Node l a r]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

-- tests
type TI = Identity
type TC = Constant Int
type TO = Optional
type TL = List
type TT = Three Int Int
type TP = Pair Int
type TB = Big Int
type TBB = Bigger Int
type TS = S []
type TTree = Tree

main = do
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TC (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TO (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TL (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TT (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TP (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TB (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TBB (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TS (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)
  let trigger :: TTree (Int, Int, [Int])
      trigger = undefined 
  quickBatch (traversable trigger)