module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTripAB :: (Show a, Read b) => a -> b
roundTripAB a = read (show a)

main = do
    print (roundTrip 4)
    print (id 4)
    print (roundTripPF 4)
    print ((roundTripAB 4)::Integer)