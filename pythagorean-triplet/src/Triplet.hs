module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = a^2 + b^2 == c^2

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (x, y, z)
  where [x,y,z] = sort [a,b,c]

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = filter isPythagorean permutations
  where range = [minFactor..maxFactor]
        permutations = [ (x, y, z) | x <- range
                                   , y <- range
                                   , z <- range
                                   , (x < y && y < z) ]
