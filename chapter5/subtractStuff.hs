-- this is not correct, but is an example of
-- how currying works

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10 
subtractOne = subtractStuff 1 