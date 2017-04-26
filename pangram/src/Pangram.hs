module Pangram (isPangram) where

import Data.Char (isAlpha, toLower)
import Data.List (sort, nub)

isPangram :: String -> Bool
isPangram = ((==) ['a'..'z']) . nub . sort . map toLower . filter isAlpha
