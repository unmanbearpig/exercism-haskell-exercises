module Sieve (primesUpTo) where

import Data.List (foldl')

primesUpTo :: Integer -> [Integer]
primesUpTo n = foldl' (\a m -> filter (\x -> x `mod` m /= 0 || m == x) a) [2..n] [2..n]
