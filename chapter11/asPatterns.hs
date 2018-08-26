import Data.Char
import Data.List
import Data.List.Split

isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf l@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf l ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords str =
  map capitalize (words str)
    where capitalize w@(x:xs) = 
            (w, (toUpper x):xs)

capitalizeWord :: String -> String
capitalizeWord (x:xs) = (toUpper x) : xs

capitalizeParagraph :: String -> String
capitalizeParagraph str =
  concat $ intersperse "." (map f (splitOn "." str))
    where f [] = []
          f (x:xs)
            | isLetter x = (toUpper x) : xs
            | otherwise = x : f xs
