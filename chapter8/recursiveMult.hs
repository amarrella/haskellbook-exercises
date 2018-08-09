recursiveMult :: (Integral a) => a -> a -> a
recursiveMult x y = go x y 0 
    where go a b acc 
            |  b == 0 = acc
            |  otherwise = go a (b-1) (acc+a)