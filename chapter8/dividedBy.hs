type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy = div

dividedBy2 :: Integral a => a -> a -> (a, a)
dividedBy2 num denom = go num denom 0
  where   go n d count
       | n < d = (count, n)
       | otherwise =
        go (n - d) d (count + 1)

data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy3 ::  Numerator -> Denominator -> (DividedResult, Integer)
dividedBy3 num denom = go num denom 0
  where   go n d count
       | d == 0 = (DividedByZero, 0)
       | n < 0 && d > 0 = go (-n) d count
       | d < 0 && n > 0 = go n (-d) count
       | n < 0 && d < 0 = go (-n) (-d) count
       | n < d = (if (num < 0 && denom < 0) || (num > 0 && denom > 0) then Result count else Result (-count), n)
       | otherwise =
        go (n - d) d (count + 1)