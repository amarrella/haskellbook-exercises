addExclamationPoint a = a ++ "!"
return5thChar a = a !! 4
drop9Char a = drop 9 a

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs a = 
    drop 9 a ++ take 4 (drop 5 a) ++ take 5 a
