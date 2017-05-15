{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Universe.Helpers (choices)
import Data.List (find)

newtype FactorPair a = FactorPair (a, a) deriving (Functor, Foldable, Show)

toTuple :: FactorPair a -> (a, a)
toTuple (FactorPair t) = t

factorPair :: Integer -> Integer -> FactorPair Integer
factorPair x fact = FactorPair (fact, x `div` fact)

-- I think it should return Maybe (Integer, [(Integer, Integer)])
-- because there might not be a palindrome in the provided range of factors
largestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
largestPalindrome minFact maxFact = intPairsToIntTuples (pal, factorsInRange minFact maxFact pal)
  where pal = fromJust $ find isPal $ products $ reverse [minFact..maxFact]

smallestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
smallestPalindrome minFact maxFact = intPairsToIntTuples (pal, factorsInRange minFact maxFact pal)
  where pal = fromJust $ find isPal $ products [minFact..maxFact]

products :: [Integer] -> [Integer]
products range = product <$> choices [range, range]

intPairsToIntTuples :: (Integer, [FactorPair Integer]) -> (Integer, [(Integer, Integer)])
intPairsToIntTuples (x, fp) = (x, toTuple <$> fp)

factorsInRange :: Integer -> Integer -> Integer -> [FactorPair Integer]
factorsInRange rMin rMax x = filter (all (inRange rMin rMax)) $ factorPair x <$> factors x

inRange :: Integer -> Integer -> Integer -> Bool
inRange rMin rMax x = x >= rMin && x <= rMax

-- converting to Text and then reversing seems faster that reversing String
isPal :: (Show a, Integral a) => a -> Bool
isPal x = if x `mod` 10 == 0
          then False
          else str == T.reverse str
  where str = T.pack $ show x

factors :: Integral a => a -> [a]
factors 1 = [1]
factors x = filter ((== 0) . mod x) [1 .. (x `div` 2)]
