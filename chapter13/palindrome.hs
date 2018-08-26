import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let normalized = filter isLetter . map toLower $ line1
  case (normalized == reverse normalized) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!" 
                exitSuccess