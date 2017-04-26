module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference x = squareOfSums x - sumOfSquares x

numbersUpTo x = [1..x]

squareOfSums :: Integral a => a -> a
squareOfSums = (^2) . sum . numbersUpTo

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . map (^2) . numbersUpTo
