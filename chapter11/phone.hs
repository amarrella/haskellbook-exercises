import Data.Char
import Data.List

type Digit = Char
type Presses = Int

data Button = Button Digit String
data DaPhone = DaPhone [Button]

phone = DaPhone [
  Button '1' "1",
  Button '2' "abc2",
  Button '3' "def3",
  Button '4' "ghi4",
  Button '5' "jkl5",
  Button '6' "mno6",
  Button '7' "pqrs7",
  Button '8' "tuv8",
  Button '9' "wxyz9",
  Button '*' "^*",
  Button '0' "+ 0",
  Button '#' ".,#"
  ]

convo :: [String] 
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps p@(DaPhone ((Button d s):xs)) c
  | isUpper c = ('*', 1) : reverseTaps p (toLower c)
  | elem c s = [(d, presses c s)]
  | otherwise = reverseTaps (DaPhone xs) c
      where presses c (y:ys)
              | y == c = 1
              | otherwise = 1 + presses c ys

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead p s =
  concat . map (reverseTaps p) $ s

convertFullConvo :: DaPhone
                 -> [String]
                 -> [(Digit, Presses)]
convertFullConvo p s =
  concat . map (cellPhonesDead p) $ s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps s = 
  foldr sumPresses 0 s
    where sumPresses (_, p) b = p + b

mostPopularLetter :: String -> Char
mostPopularLetter msg@(x:xs) =
  foldr best x xs
  where occ c = length . filter (==c)
        best c1 c2
          | occ c1 msg > occ c2 msg = c1
          | otherwise = c2

letterCost :: DaPhone -> Char -> Int
letterCost p =
  fingerTaps . reverseTaps p

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord conv =
  foldr best w ws
  where all@(w:ws) = words $ concat $ intersperse " " conv 
        occ c = length . filter (==c)
        best c1 c2
          | occ c1 all > occ c2 all = c1
          | otherwise = c2
