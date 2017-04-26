module Atbash (decode, encode) where

import Data.List (intercalate)
import Data.Char

data AlphabetSpec = AlphabetSpec { aOffset :: Int, aLength :: Int }
english = AlphabetSpec 97 25

decode :: String -> String
decode = map (encodeChar english) . filter isAlphaNum

encode :: String -> String
encode = intercalate " " . splitAtbash 5 . map (encodeChar english) . map toLower . filter isAlphaNum


encodeChar :: AlphabetSpec -> Char -> Char
encodeChar alphaSpec c
  | n >= alphaOffset && n <= (alphaOffset + alphaLength) = chr $ alphaInverse
  | otherwise = c
  where n = ord c
        alphaInverse  = alphaOffset + (alphaLength - alphaNum)
        alphaNum      = n - alphaOffset
        alphaLength   = aLength alphaSpec
        alphaOffset   = aOffset alphaSpec

splitAtbash :: Int -> String -> [String]
splitAtbash _ [] = []
splitAtbash n xs = grp : splitAtbash n rest
  where (grp, rest) = splitAt n xs
