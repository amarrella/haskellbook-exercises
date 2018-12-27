
module Morra where

import Control.Monad.State
import Control.Monad.Trans.Except
import Text.Read (readEither)
import System.Random

data Scores = Scores Integer Integer deriving (Show)

type Game = StateT Scores (ExceptT String IO) Scores

data Mode = CPU | Human deriving (Show)
data Play = Play {
  getInput :: Integer,
  getTotal :: Integer
} deriving (Show)

instance Semigroup Scores where
  (Scores x1 y1) <> (Scores x2 y2) =
    Scores (x1 + x2) (y1 + y2)

instance Monoid Scores where
  mempty = Scores 0 0
  mappend = (<>)
    
getMode :: ExceptT String IO Mode
getMode = do
  liftIO $ putStrLn "Select mode: 1(CPU) 2"
  liftIO $ putStrLn "1: CPU"
  liftIO $ putStrLn "2: Human"
  line <- liftIO getLine
  case line of
    "1" -> return CPU
    "2" -> return Human
    _ -> ExceptT $ return (Left "Invalid mode selected")

readNumber :: ExceptT String IO Integer
readNumber = do
  line <- liftIO getLine
  ExceptT $ return (readEither line)

play :: ExceptT String IO Play
play = do
  liftIO $ putStr "Select a number between 1 and 5: "
  input <- readNumber
  validatedInput <- ExceptT $ 
    if input >= 1 && input <= 5 then
      return $ Right input 
    else 
      return $ Left "Invalid input. Insert a number between 1 and 5" 
  liftIO $ putStr "What is the total gonna be? Select a number between 2 and 10: "
  total <- readNumber
  validatedTotal <- ExceptT $ 
    if total >= 2 && total <= 10 then
      return $ Right total 
    else 
      return $ Left "Invalid total. Insert a number between 2 and 10" 
  return $ Play validatedInput validatedTotal

cpuPlay :: IO Play
cpuPlay = do
  input <- randomRIO (1, 5)
  total <- randomRIO (2, 10)
  return $ Play input total

turn :: Mode -> ExceptT String IO Scores
turn mode = do
  player1 <- play
  player2 <- case mode of 
    Human -> play
    CPU -> lift cpuPlay
  let p1Total = getTotal player1
  let p2Total = getTotal player2
  let total = (getInput player1) + (getInput player2)
  let scores
        | p1Total == total && p2Total == total  = Scores 1 1
        | p1Total == total                      = Scores 1 0
        | p2Total == total                      = Scores 0 1
        | otherwise                             = Scores 0 0
  return scores

run :: Mode -> Game
run mode = do
  state <- get
  score <- lift $ turn mode
  let totalScore = mappend state score
  liftIO $ putStr "Continue? Y to continue: "
  line <- liftIO getLine
  case line of
    "Y" -> put totalScore >> (run mode)
    _ -> return totalScore

main :: IO ()
main = do
  liftIO $ putStrLn "Let's play Morra!"
  result <- runExceptT $ do
    m <- getMode
    evalStateT (run m) mempty   
  case result of
    Left e -> putStrLn e
    Right (Scores s1 s2) ->
      let msg
            | s1 > s2   = "Player 1 wins!"
            | s1 == s2  = "It's a draw"
            | otherwise = "Player 2 wins!" 
      in do
        putStr "Player 1: "
        print s1
        putStr "Player 2: "
        print s2
        putStrLn msg