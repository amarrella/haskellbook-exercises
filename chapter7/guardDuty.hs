-- Shows why it's bad to have otherwise as first guard
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | otherwise = 'F'
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    where y = x / 100

-- Shows that evaluation of guards is sequential
avgGrade2 :: (Fractional a, Ord a) => a -> Char
avgGrade2 x
    | y >= 0.7  = 'C'
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.59 = 'D'
    | otherwise = 'F'
    where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
    | xs == reverse xs  = True
    | otherwise         = False

numbers :: (Num a, Ord a) => a -> Integer
numbers x 
    | x < 0     = -1
    | x == 0    = 0
    | x > 0     = 1