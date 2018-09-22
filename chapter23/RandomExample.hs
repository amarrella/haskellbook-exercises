module RandomExample where

import Control.Applicative (liftA3) 
import Control.Monad (replicateM) 
import Control.Monad.Trans.State 
import System.Random
  
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die 
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> 
    error $
      "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die 
rollDie = state $ do
  (n, s) <- randomR (1, 6) 
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die) 
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int 
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int 
    go sum count gen
      | sum >= 20 = count 
      | otherwise =
        let (die, nextGen) = 
              randomR (1, 6) gen
        in go (sum + die) 
              (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int 
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int 
    go sum count gen
      | sum >= n = count 
      | otherwise =
        let (die, nextGen) = 
              randomR (1, 6) gen
        in go (sum + die) 
              (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die]) 
rollsCountLogged n g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int , [Die])
    go sum count rolls gen
      | sum >= n = (count, rolls) 
      | otherwise =
        let (die, nextGen) = 
              randomR (1, 6) gen
        in go (sum + die) 
              (count + 1) (rolls ++ [intToDie die]) nextGen

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi (\x -> let (a, s) = g x in (f a, s))

instance Applicative (Moi s) where 
  pure a = Moi (\s -> (a, s))
  (Moi fas) <*> (Moi g) = Moi (\x -> 
    let (fa, s)   = fas x 
        (a, s')   = g s
    in (fa a, s))

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi $ \x ->
    let (a, s) = f x
        mb = g a
    in runMoi mb s


main = undefined