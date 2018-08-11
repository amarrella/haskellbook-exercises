mult3 :: (Integral a) => [a] -> [a]
mult3 xs = filter (\x -> x `mod` 3 == 0) xs

nMult3 :: (Integral a) => [a] -> Int
nMult3 = length . mult3

myFilter :: [Char] -> [[Char]]
myFilter = filter (\x -> not $ elem x ["the", "a", "an"]) . words