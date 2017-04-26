module WordCount (wordCount) where

import Data.List (foldl', sort, last)
import Data.List.Split (wordsBy)
import Data.Char (isAlphaNum, toLower)

type WordCount = (String, Int)

wordCount :: String -> [WordCount]
wordCount = foldl' addCount []
  . sort
  . filter (not . null . fst)
  . map initWordCount
  . map trimQuotes
  . wordsBy (not . isWordChar)
  . map toLower

isQuote :: Char -> Bool
isQuote c
  | c == '\''    = True
  | c == '"'     = True
  | otherwise    = False

isWordChar :: Char -> Bool
isWordChar c
  | isAlphaNum c = True
  | isQuote c    = True
  | otherwise    = False

trimQuotes :: String -> String
trimQuotes  [] = []
trimQuotes (h:xs) = if isQuote (last headString) then take (length headString - 1) headString else headString
  where headString = if isQuote h then xs else h:xs

initWordCount :: String -> WordCount
initWordCount s = (s, 1)

addCount :: [WordCount] -> WordCount -> [WordCount]
addCount [] wc = [wc]
addCount cs@((w',n'):xs) b@(w,n)
  |   w == w' = (w, n' + n):xs
  | otherwise = b:cs
