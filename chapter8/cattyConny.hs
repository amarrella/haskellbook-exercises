cattyConny :: String -> String -> String
cattyConny x y = x ++ "mrow" ++ y

flippy :: [Char] -> [Char] -> [Char]
flippy = flip cattyConny

appedCatty :: [Char] -> [Char]
appedCatty = cattyConny "woops"

frappe :: [Char] -> [Char]
frappe = flippy "haha"