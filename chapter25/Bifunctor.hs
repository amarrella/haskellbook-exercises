module Bifunctor where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second f = bimap id f

data Deux a b = Deux a b

instance Bifunctor Deux where
 first f (Deux a b) = Deux (f a) b
 second f (Deux a b) = Deux a (f b)

data Const a b = Const a

instance Bifunctor Const where
  first f (Const a) = Const (f a)
  second _ (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  first f (Drei a b c) = Drei a (f b) c
  second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  first f (SuperDrei a b) = SuperDrei a (f b)
  second _ (SuperDrei a b) = SuperDrei a b

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = 
    Quadzzz a b (f c) (g d)

data Either' a b =
    Left' a
  | Right' b

instance Bifunctor Either' where
  first f (Left' a) = Left' (f a)
  first _ (Right' a) = Right' a
  second f (Left' a) = Left' a
  second f (Right' a) = Right' (f a)