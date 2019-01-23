module Vigenere where

import System.Environment (getArgs)
import System.IO (hIsEOF, hGetChar, hPutStr, stdin, stdout, Handle)
import qualified Cipher as C

data Mode = Encrypt | Decrypt deriving Show
data Args = Args { getMode :: Mode, getKey :: C.Key }

parse :: [String] -> Args
parse ["-e", k] = Args Encrypt (C.Key k)
parse ["-d", k] = Args Decrypt (C.Key k)
parse _ = error "Invalid input"

readInput :: Handle -> IO String
readInput h = go []
  where go input = do
          eof <- hIsEOF h
          if (eof) then pure (reverse input) else do
              c <- hGetChar h
              go (c:input)

main :: IO ()
main = do
  args <- getArgs
  let parsed = parse args
  let mode = getMode parsed
  let key = getKey parsed
  input <- readInput stdin
  let output = case mode of 
                Encrypt -> C.getContent $ C.vigenere key (C.Msg input)
                Decrypt -> C.getMsg $ C.unVigenere key (C.Encrypted input)
  hPutStr stdout output
  