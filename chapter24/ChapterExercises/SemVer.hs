module ChapterExercises.SemVer where

import Control.Applicative
import Text.Trifecta

data NumberOrString = 
    NOSS String
  | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
  (SemVer maj1 min1 patch1 _ _) `compare` (SemVer maj2 min2 patch2 _ _) =
    (maj1, min1, patch1) `compare` (maj2, min2, patch2)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = 
  NOSI <$> decimal <|> NOSS <$> some alphaNum

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  rel <- option [] $ char '-' >> sepBy1 parseNumberOrString (char '.')
  metadata <- option [] $ char '+' >> sepBy1 parseNumberOrString (char '.')
  return $ SemVer major minor patch rel metadata