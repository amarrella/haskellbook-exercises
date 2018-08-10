splitString :: String -> Char -> [String]
splitString s c = go s []
    where go str acc
        | str == "" = reverse acc
        | otherwise = go (drop 1 (dropWhile (/= c) str)) ((takeWhile (/= c) str) : acc)
