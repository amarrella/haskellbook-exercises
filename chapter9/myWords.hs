myWords :: String -> [String]
myWords s = go s []
    where go str acc 
        | str == "" = reverse acc
        | otherwise = go (drop 1 (dropWhile (/= ' ') str)) ((takeWhile (/= ' ') str) : acc)