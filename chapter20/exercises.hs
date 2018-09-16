-- 1
data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldMap _ _ = mempty

-- 2
data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 3
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4
data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = (f b) <> (f b')

-- 5
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') =
    (f b) <> (f b') <> (f b'')

-- 6
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF pred = foldMap (\a -> if pred a then pure a else mempty)