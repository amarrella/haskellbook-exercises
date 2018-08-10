sumToN :: (Eq a, Num a) => a -> a
sumToN n = go 0 n 
      where go acc x
          | x == 0 = acc
          | otherwise = go (acc + x) (x-1)