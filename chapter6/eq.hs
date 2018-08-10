data TisAnInteger = 
  TisAn Integer 

instance Eq TisAnInteger where 
  (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt =
  TisAnInt  Int 
  | TisAString  String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v v') (Pair w w') = v == w && v' == w'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple v v') (Tuple w w') = v == w && v' == w'

data Which a =
  ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where 
  (==) (ThisOne v) (ThisOne v') = v == v'
  (==) (ThatOne v) (ThatOne v') = v == v'
  (==) _ _ = False

data EitherOr a b =
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v')     = v == v'
  (==) (Goodbye v) (Goodbye v')   = v == v'
  (==) _ _ = False
