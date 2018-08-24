data Quad = One | Two | Three | Four deriving (Eq, Show)


-- how many different forms? 

-- Sum of products (4 * 1) + (4 * 1) = 8
eQuad :: Either Quad Quad
eQuad = undefined

-- Product 4 * 4 = 16
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- Exponent 4^4 = 256
funcQuad :: Quad -> Quad
funcQuad = undefined

-- Product 2 * 2 * 2 = 8
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- Exponent (2 ^ 2) ^ 2
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- Exponent (4 ^ 4) ^ 2
fTwo :: Bool -> Quad -> Quad
fTwo = undefined