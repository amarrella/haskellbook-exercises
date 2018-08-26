newtype Word' = 
  Word' String 
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str =
  if (countCons str > countVowels str) then
    Just (Word' str)
  else
    Nothing
  where
    countVowels = length . filter (`elem` vowels)
    countCons = length . filter (\x -> not (elem x vowels))