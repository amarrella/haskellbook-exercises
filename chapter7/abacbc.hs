g :: (a -> b) -> (a, c) -> (b,  c)
g aToB ac = 
  case ac of 
    (a, c) -> (aToB a, c)