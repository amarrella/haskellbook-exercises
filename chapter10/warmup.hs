stops = "pbtdkg"
vowels = "aeiou"

combinations = [(x,y,z) | 
  x <- stops, 
  y <- vowels,
  z <- stops
  ]

beginningWithP = filter (\(x,y,z) -> x == 'p') combinations

nouns = ["car", "dog", "house", "chair"]
verbs = ["like", "sit", "sleep", "go"]

combinationsS = [(x,y,z) | 
  x <- nouns, 
  y <- verbs,
  z <- nouns
  ]


-- finds the average length of a word in a space-separated sentence
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritFuncFrac x =
  fromIntegral(sum (map length (words x))) / fromIntegral(length (words x))
