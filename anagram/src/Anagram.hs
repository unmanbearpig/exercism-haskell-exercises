module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\candidate -> normalized == (sort $ map toLower candidate))
                 . filter (\candidate -> (map toLower candidate) /= lowercased)
  where normalized = sort $ lowercased
        lowercased = map toLower xs
