import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $ 
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  age <- getLine
  let ageInt = read age::Integer
  let person = mkPerson name ageInt
  case person of
    Right p -> do putStrLn "Yay! Successfully\
                          \ got a person!"
                  putStrLn (show person)
    Left e -> do putStrLn "An error occurred"
                 putStrLn (show e)

