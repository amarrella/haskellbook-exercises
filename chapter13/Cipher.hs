module Cipher where

import Data.Char
import System.IO

newtype Key = Key String
newtype Msg = Msg { getMsg :: String } deriving Show
newtype Encrypted = Encrypted { getContent :: String } deriving Show

zipKey :: Key -> String -> [(Char, Char)]
zipKey _ [] = []
zipKey (Key k) (x:xs)
  | isLetter x = (x, y) : zipKey (Key ys) xs
  | otherwise = (x, x) : zipKey (Key (y:ys)) xs
  where y:ys = concat $ repeat k

shift :: Char -> Int
shift x 
  | elem x ['a'..'z'] = (ord x - ord 'a')
  | elem x ['A'..'Z'] = (ord x - ord 'A')

vigenere :: Key -> Msg -> Encrypted
vigenere k (Msg m) =
  Encrypted (map f (zipKey k m))
    where f (a, b)
            | elem a ['a'..'z'] = 
              chr (((shift a + shift b) `mod` 26) + ord 'a')
            | elem a ['A'..'Z'] = 
              chr (((shift a + shift b) `mod` 26) + ord 'A')
            | otherwise = a

unVigenere :: Key -> Encrypted -> Msg
unVigenere k (Encrypted e) =
  Msg (map f (zipKey k e))
    where f (a, b)
            | elem a ['a'..'z'] = 
              chr (((shift a - shift b) `mod` 26) + ord 'a')
            | elem a ['A'..'Z'] = 
              chr (((shift a - shift b) `mod` 26) + ord 'A')
            | otherwise = a

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Input message to encrypt: "
  w <- getLine
  putStr "Input key: "
  n <- getLine
  let encrypted = vigenere (Key n) (Msg w)
  print $ encrypted
  print $ unVigenere (Key n) encrypted