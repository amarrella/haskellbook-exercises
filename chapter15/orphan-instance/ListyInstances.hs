module ListyInstances where

import Data.Monoid
import Listy

-- Causes duplicate instance declarations error
--instance Semigroup (Listy a) where
--  (<>) (Listy l) (Listy l') =
--    Listy $ mappend l l'

--instance Monoid (Listy a) where 
--  mempty = Listy []