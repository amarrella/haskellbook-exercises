module Main where
import Criterion.Main
import qualified Data.Sequence as S

-- Queue 
data Queue a =
  Queue { enqueue :: [a] , dequeue :: [a]
  } deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue e d) = Queue (a:e) d

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] [])   = Nothing
pop (Queue e (x:xs))  = Just (x, Queue e xs)
pop (Queue xs []) = pop (Queue [] (reverse xs))

-- Queue with single list
newtype QueueS a =
  QueueS [a] 
  deriving (Eq, Show)

emptyS :: QueueS a
emptyS = QueueS []

pushS :: a -> QueueS a -> QueueS a
pushS a (QueueS as) = QueueS (a:as)

popS :: QueueS a -> Maybe (a, QueueS a)
popS (QueueS [])   = Nothing
popS (QueueS as)   = Just(x, QueueS xs) 
  where (x:xs) = reverse as

-- Sequence

pushSeq :: a -> S.Seq a -> S.Seq a
pushSeq = (S.<|)

popSeq :: S.Seq a -> Maybe (a, S.Seq a)
popSeq S.Empty   = Nothing
popSeq ( as S.:|> a) = Just(a, as)

-- Benchmark utils
popAll :: Queue a -> Maybe a
popAll qa =
  case pop qa of 
    Nothing -> Nothing
    Just (x, qa') -> popAll qa'

pushPop :: [a] -> Maybe a
pushPop as = 
  let q = foldr push empty as in
    popAll q

popAllS :: QueueS a -> Maybe a
popAllS qa =
  case popS qa of 
    Nothing -> Nothing
    Just (x, qa') -> popAllS qa'

pushPopS :: [a]-> Maybe a
pushPopS as =
  let q = foldr pushS emptyS as in popAllS q

popAllSeq :: S.Seq a -> Maybe a
popAllSeq as =
  case popSeq as of 
    Nothing -> Nothing
    Just (x, qa') -> popAllSeq qa'

pushPopSeq :: [a] -> Maybe a
pushPopSeq as = 
  let q = foldr pushSeq S.Empty as in
    popAllSeq q
  
main :: IO ()
main = defaultMain
  [ bench "pushPop Queue" $ whnf pushPop [1..100]
  , bench "pushPop QueueS" $ whnf pushPopS [1..100]
  , bench "pushPop Seq" $ whnf pushPopSeq [1..100]
  ]