module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar n str = 
  map cipher str
      where cipher a
              | elem a ['a'..'z'] = 
                chr ((((ord a - ord 'a') + n) `mod` 26) + ord 'a')
              | elem a ['A'..'Z'] = 
                chr ((((ord a - ord 'A') + n) `mod` 26) + ord 'A')

unCaesar :: Int -> String -> String
unCaesar n str = caesar (-n) str