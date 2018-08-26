module Cipher where

import Data.Char

newtype Key = Key String
newtype Msg = Msg String deriving Show
newtype Encrypted = Encrypted String deriving Show

zipKey :: Key -> Msg -> [(Char, Char)]
zipKey _ (Msg [])= []
zipKey (Key k) (Msg (x:xs))
  | isLetter x = (x, y) : zipKey (Key ys) (Msg xs)
  | otherwise = (x, x) : zipKey (Key (y:ys)) (Msg xs)
  where y:ys = concat $ repeat k

shift :: Char -> Int
shift x 
  | elem x ['a'..'z'] = (ord x - ord 'a')
  | elem x ['A'..'Z'] = (ord x - ord 'A')

vigenere :: Key -> Msg -> Encrypted
vigenere k m =
  Encrypted (map f (zipKey k m))
    where f (a, b)
            | elem a ['a'..'z'] = 
              chr (((shift a + shift b) `mod` 26) + ord 'a')
            | elem a ['A'..'Z'] = 
              chr (((shift a + shift b) `mod` 26) + ord 'A')
            | otherwise = a