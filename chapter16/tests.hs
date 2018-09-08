{-# LANGUAGE ViewPatterns #-}

import Laws
import FunctorExercises
import Possibly
import Sum
import ChapterExercises
import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type FCInt a = a -> IntToInt -> IntToInt -> Bool

main :: IO () 
main = do
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose (+1) (*2) :: [Int] -> Bool)
  quickCheck (functorCompose' :: FCInt [Int])
  quickCheck (functorIdentity :: (Identity Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Identity Int))
  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Pair Int))
  quickCheck (functorIdentity :: (Two String Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Two String Int))
  quickCheck (functorIdentity :: (Three String String Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Three String String Int))
  quickCheck (functorIdentity :: (Three' String Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Three' String Int))
  quickCheck (functorIdentity :: (Four String Int String Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Four String Int String Int))
  quickCheck (functorIdentity :: (Four' String Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Four' String Int))
  quickCheck (functorIdentity :: (Possibly Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Possibly Int))
  quickCheck (functorIdentity :: (Sum String Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (Sum String Int))
  quickCheck (functorIdentity :: (BoolAndSomethingElse Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (BoolAndSomethingElse Int))
  quickCheck (functorIdentity :: (BoolAndMaybeSomethingElse Int) -> Bool)
  quickCheck (functorCompose' :: FCInt (BoolAndMaybeSomethingElse Int))
