import Data.Char

filterUppercase :: String -> String
filterUppercase str = filter isUpper str

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

allCaps :: String -> String
allCaps [] = []
allCaps (x:xs) = toUpper x : allCaps xs

capitalizeHead :: String -> Char
capitalizeHead str = toUpper (head str)

capitalizeHeadCompose :: String -> Char
capitalizeHeadCompose str = toUpper . head $ str

capitalizeHeadPF :: String -> Char
capitalizeHeadPF = toUpper . head