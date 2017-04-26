module SumOfMultiples (sumOfMultiples) where

isMultiple :: [Integer] -> Integer -> Bool
isMultiple xs y = any (\ x -> y `mod` x == 0) xs

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples xs upTo = sum $ filter (isMultiple xs) ys
  where ys = takeWhile (< upTo) $ iterate (+ 1) 1