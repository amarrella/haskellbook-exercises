module Main where
import Criterion.Main

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

main :: IO ()
main = defaultMain
  [ bench "pushPop Queue" $ whnf pushPop [1..100]
  , bench "pushPop QueueS" $ whnf pushPopS [1..100]
  ]