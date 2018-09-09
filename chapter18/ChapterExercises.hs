module ChapterExercises where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ n = NopeDotJpg

instance Applicative Nope where
  pure  _  = NopeDotJpg
  _ <*> n  = NopeDotJpg

instance Monad Nope where
  return  = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

nope :: Nope (String, String, String)
nope = undefined

-- 2
data PhhhbbtttEither b a = 
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure                        = Left'
  (Right' b)  <*> _           = Right' b
  _           <*> (Right' b)  = Right' b
  (Left' f)   <*> (Left' a)   = Left' (f a)

instance Monad (PhhhbbtttEither b) where
  return                    = pure
  (Right' b) >>= _          = Right' b
  (Left' a)  >>= f          = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' a, Right' b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

pEither :: PhhhbbtttEither Int (String, String, String)
pEither = undefined 

-- 3
newtype Identity a = 
  Identity a 
  deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where 
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where 
  return = pure
  (Identity a) >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identity :: Identity (String, String, String)
identity = undefined

-- 4
data List a =
    Nil
  | Cons a (List a) deriving (Eq, Show)

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
flatMap f as = concat' $ fmap f as

instance Applicative List where 
  pure a      = Cons a Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  fs <*> xs = 
    flatMap (`fmap` xs) fs

instance Monad List where
  return             = pure
  Nil         >>= _  = Nil
  as >>= f           = flatMap f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a   <- arbitrary
    as  <- arbitrary
    elements [Nil, Cons a as]

instance Eq a => EqProp (List a) where
  (=-=) = eq

list :: List (String, String, String)
list = undefined

----------------
-- SECOND BLOCK
----------------

-- 1
j :: Monad m => m (m a) -> m a
j mma = mma >>= id

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb =
  mb >>= (\b -> (ma >>= \a -> return $ f a b))

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = 
  mf <*> ma

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = do
  b   <- f a
  bs  <- meh as f
  return $ b:bs
  
-- 6
flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id

main = do
  putStrLn "1"
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope
  putStrLn "2"
  quickBatch $ functor pEither
  quickBatch $ applicative pEither
  quickBatch $ monad pEither
  putStrLn "3"
  quickBatch $ functor identity
  quickBatch $ applicative identity
  quickBatch $ monad identity
  putStrLn "4"
  quickBatch $ functor list
  quickBatch $ applicative list
  quickBatch $ monad list