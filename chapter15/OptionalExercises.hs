module OptionalExercises where 
import Test.QuickCheck
import Laws

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

-- Making it work with ghc 8.4.1+
instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada         Nada        = Nada
  (<>) Nada         (Only y)    = Only y
  (<>) (Only x)     Nada        = Only x
  (<>) (Only x)     (Only y)    = Only(x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty                          = Nada

newtype First' a =
  First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only x))  _
    = First' (Only x)
  (<>) (First' Nada) (First' (Only y))
    = First' (Only y)
  (<>) (First' Nada) (First' Nada)
    = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend  :: First' a
              -> First' a
              -> First' a
firstMappend = mappend

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ First' (Only a)),
              (1, return $ (First' Nada))]

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool