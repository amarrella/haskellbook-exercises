module ChapterExercises where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . ((-1) +)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  x <- ask
  liftIO $ print ("Hi: " ++ show x)
  return $ (x+1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  s <- get
  liftIO $ print ("Hi: " ++ show s)
  put (s+1)
  return $ show s

isValid :: String -> Bool 
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine 
  guard $ isValid v 
  return v

doExcite :: IO () 
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE" 
    Just e -> putStrLn ("Good, was very excite: " ++ e)