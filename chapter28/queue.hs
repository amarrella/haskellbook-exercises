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

pushPop :: a -> Queue a -> a
pushPop a qa = x
  where Just(x, _) = pop (push a qa)

pushPopS :: a -> QueueS a -> a
pushPopS a qa = x
  where Just(x, _) = popS (pushS a qa)

main :: IO ()
main = defaultMain
  [ bench "pushPop Queue" $ whnf pushPop 1000
  , bench "pushPop QueueS" $ whnf pushPopS 1000
  ]