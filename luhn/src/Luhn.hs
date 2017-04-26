module Luhn (addends, checkDigit, checksum, create, isValid) where

import Data.List (unfoldr)

withIndicies :: [a] -> [(a, Int)]
withIndicies xs = zip xs [1..]

addends :: Integer -> [Integer]
addends = reverse . map (\(x, i) -> if i `mod` 2 == 0 then lunhDouble x else x) . withIndicies . toDigits

lunhDouble x = if dbl > 9 then dbl - 9 else dbl
  where dbl = x * 2

toDigits :: Integer -> [Integer]
toDigits = unfoldr nextDigit
  where nextDigit :: Integer -> Maybe (Integer, Integer)
        nextDigit 0 = Nothing
        nextDigit x = Just (x `mod` 10, x `div` 10)

checkDigit :: Integer -> Integer
checkDigit n = n `mod` 10

checksum :: Integer -> Integer
checksum = (flip mod 10) . sum . addends

create :: Integer -> Integer
create n = n * 10 + (10 - checksum (n * 10)) `mod` 10

isValid :: Integer -> Bool
isValid n = checksum n == 0
