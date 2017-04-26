module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isAlpha)

isQuestion :: String -> Bool
isQuestion string = (last string) == '?'

isYelling :: String -> Bool
isYelling string = (any isAlpha string) &&
                   (all isUpper $ filter isAlpha string)

isSilence :: String -> Bool
isSilence [] = True
isSilence string = all isSpace string

responseFor :: String -> String
responseFor string
  | isSilence string = "Fine. Be that way!"
  | isYelling string = "Whoa, chill out!"
  | isQuestion string = "Sure."
  | otherwise = "Whatever."