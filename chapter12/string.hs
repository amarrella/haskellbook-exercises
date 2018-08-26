import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

replaceThe :: String -> String
replaceThe str =
  unwords (map (replaceThe . notThe) (words str))
      where
        replaceThe Nothing = "a"
        replaceThe (Just x) = x

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel str = 
  if(x == "the" && followedByVowel xs) then 
    1 + countTheBeforeVowel (unwords xs)
  else
    countTheBeforeVowel (unwords xs)
  where 
    (x:xs) = words str
    followedByVowel [] = False
    followedByVowel (y:ys) = 
      elem (head y) "aeiouAEIOU"
  
countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` "aeiouAEIOU")