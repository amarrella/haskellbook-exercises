import OptionalExercises
import SemigroupExercises
import MonoidExercises
import Laws
import Test.QuickCheck
import Data.Semigroup

main :: IO () 
main = do
  quickCheck (semigroupAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool) 
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool) 
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc Trivial String)
  quickCheck (monoidLeftIdentity :: (Two String Trivial) -> Bool) 
  quickCheck (monoidRightIdentity :: (Two String Trivial) -> Bool)
  quickCheck (semigroupAssoc :: ThreeAssoc Trivial String Trivial)
  quickCheck (semigroupAssoc :: FourAssoc Trivial String Trivial String)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc String Trivial)
  --quickCheck (semigroupAssoc :: CombineAssoc (Sum Integer) (Sum Integer))
  --quickCheck (semigroupAssoc :: CompAssoc ((Sum Integer) -> (Sum Integer)))
  --quickCheck (monoidLeftIdentity :: Comp (Sum Integer) -> Bool)
  --quickCheck (monoidRightIdentity :: Comp (Sum Integer) -> Bool)
  quickCheck (semigroupAssoc :: ValidationAssoc String Integer)
  --quickCheck (semigroupAssoc :: MemAssoc String Integer)
  --quickCheck (monoidLeftIdentity :: (Mem String Integer) -> Bool)
  --quickCheck (monoidRightIdentity :: (Mem String Integer) -> Bool)
  let f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
  