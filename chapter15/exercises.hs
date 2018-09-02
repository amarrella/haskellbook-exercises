import Test.QuickCheck
-- 1
data Trivial = Trivial deriving (Eq, Show) 

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc  :: (Eq m, Semigroup m) 
                => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)
newtype FirstId a = 
  FirstId {getFirst :: Identity a} deriving (Eq, Show)
newtype LastId a = 
  LastId {getLast :: Identity a} deriving (Eq, Show)

instance Semigroup (FirstId a) where
  (FirstId x) <> _ = FirstId x
instance Semigroup (LastId a) where
  _ <> (LastId y) = LastId y

type FirstIdAssoc a =
  FirstId a -> FirstId a -> FirstId a -> Bool

type LastIdAssoc a =
  LastId a -> LastId a -> LastId a -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (FirstId a) where
  arbitrary = do
    a <- arbitrary
    return (FirstId a)

instance Arbitrary a => Arbitrary (LastId a) where
  arbitrary = do
    a <- arbitrary
    return (LastId a)

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

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: FirstIdAssoc Integer)
  quickCheck (semigroupAssoc :: LastIdAssoc Integer)
  quickCheck (semigroupAssoc :: TwoAssoc Trivial String)