module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "use a number from 0 to 9"

digits :: Int -> [Int]
digits num = go num []
        where go n acc
                | n < 10 = ((n `mod` 10) : acc)
                | otherwise = go (n `div` 10) ((n `mod` 10) : acc)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits