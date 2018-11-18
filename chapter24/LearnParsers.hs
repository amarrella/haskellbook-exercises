module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> String -> IO()
testParse p s =
  print $ parseString p mempty s

pNL s =
  putStrLn ('\n' : s)

oneEof = 
  one >> eof >> stop

oneTwoEof = 
  oneTwo >> eof >> stop

testParseString :: Parser String -> String -> IO()
testParseString p s =
  print $ parseString p mempty s

charParser :: Char -> Parser Char -- Not sure i understood the exercise
charParser = char

main = do
  pNL "stop:" 
  testParse stop "123"
  pNL "one:" 
  testParse one "123"
  pNL "one':" 
  testParse one' "123"
  pNL "oneTwo:" 
  testParse oneTwo "123"
  pNL "oneTwo':" 
  testParse oneTwo' "123"
  pNL "one fail:" 
  testParse oneEof "123"
  pNL "one two fail:" 
  testParse oneTwoEof "123"
  pNL "1" 
  testParseString (string "1") "123" 
  pNL "12" 
  testParseString (string "12") "123" 
  pNL "123" 
  testParseString (string "123") "123" 