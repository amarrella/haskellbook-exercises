import Data.Bool (bool)

negate3 :: (Num a, Eq a) => [a] -> [a]
negate3 = map (\x -> bool x (-x) (x == 3))