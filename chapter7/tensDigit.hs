tensDigit :: Integral a => a -> a
tensDigit x = d 
    where   xLast   = x `div` 10
            d       = xLast `mod` 10

tensDigitPF :: Integral a => a -> a
tensDigitPF = (`mod` 10) . (`div` 10)

tensDigitDivMod :: Integral a => a -> a
tensDigitDivMod x =  snd  ((fst (x `divMod` 10)) `divMod` 10)

tensDigitDivModPF :: Integral a => a -> a
tensDigitDivModPF = snd . (`divMod` 10) . fst . (`divMod` 10)

hunsD :: Integral a => a -> a
hunsD x = d2
    where   d = x `div` 100
            d2 = d `mod` 100

hunsDPF :: Integral a => a -> a
hunsDPF = (`mod` 100) . (`div` 100)