module Series (largestProduct) where

import Data.Char (digitToInt, isDigit)
import Data.List (subsequences)

largestProduct :: Int -> String -> Maybe Integer
largestProduct size digits = fmap (toInteger . maxProduct) . consecutiveSubsequences size =<< strToDigits =<< Just digits

maxProduct :: [[Int]] -> Int
maxProduct = maximum . map (foldl (*) 1)

consecutiveSubsequences :: Int -> [a] -> Maybe [[a]]
consecutiveSubsequences size xs
  | size > length xs = Nothing
  | size < 0 = Nothing
  | otherwise = Just $ map (take size) $ map (\n -> drop n xs) [0..((length xs) - size)]

strToDigits :: String -> Maybe [Int]
strToDigits s
  | any (not . isDigit) s = Nothing
  | otherwise = Just $ map (digitToInt) s
