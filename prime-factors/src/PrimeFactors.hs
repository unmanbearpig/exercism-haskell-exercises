module PrimeFactors (primeFactors) where

import Control.Monad ((=<<), join)
import Data.Function (fix)
import Data.List (find)

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors x = if isPrime then (pure x) else f : (primeFactors d)
  where f = takeFactor x
        d = x `div` f
        isPrime = x == f

takeFactor :: Integer -> Integer
takeFactor x = maybe x id (find (\y -> x `rem` y == 0) $ [2..x])
