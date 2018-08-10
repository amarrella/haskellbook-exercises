eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool _ _ = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y 
    | x > y = []
    | x == y = [x]
    | otherwise = x : (eftOrd (succ x) y)

eftInt :: Int -> Int -> [Int]
eftInt x y 
    | x > y = []
    | x == y = [x]
    | otherwise = x : (eftInt (succ x) y)

eftChar :: Char -> Char -> [Char]
eftChar x y 
    | x >= y = []
    | x == y = [x]
    | otherwise = x : (eftChar (succ x) y)
