module Config where

import System.Environment (getArgs)
import qualified Data.Ini as I
import System.Directory (getDirectoryContents, withCurrentDirectory)
import qualified Data.ByteString as BS
import Text.Trifecta
import Data.List (isSuffixOf)

parseArgs :: [String] -> String
parseArgs [] = error "No directory specified"
parseArgs [dir] = dir
parseArgs _ = error "Invalid input"

isIni :: String -> Bool
isIni s = ".ini" `isSuffixOf` s

parseIniString :: BS.ByteString -> Result I.Config
parseIniString s = parseByteString I.parseIni mempty s

main :: IO ()
main = do
  args <- getArgs
  let dir = parseArgs args
  files <- getDirectoryContents dir
  let inis = filter isIni files
  readInis <- withCurrentDirectory dir $ traverse BS.readFile inis
  let parsed = fmap parseIniString readInis
  print parsed