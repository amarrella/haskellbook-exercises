foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    False -> x
    True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b 
    | b = y
    | otherwise = x