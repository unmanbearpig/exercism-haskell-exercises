module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper, isLower, isAlpha)
import Data.List (nubBy, groupBy)

abbreviate :: String -> String
abbreviate = concatMap abbreviateWord . words

abbreviateWord :: String -> String
abbreviateWord s = if not $ any isUpper s
                   then map toUpper groupLetters
                   else filter isUpper $ groupLetters
  where groupLetters = filter isAlpha $ map head $ groupBy groupEquality s
        groupEquality x y = f x == f y
          where f x = (isUpper x, isAlpha x)
