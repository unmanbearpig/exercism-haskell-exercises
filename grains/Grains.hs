module Grains (square, total) where

square :: Integer -> Integer
square x = 2 ^ (x-1)

totalSquare :: Integer -> Integer
totalSquare x = 2 ^ x - 1

total :: Integer
total = totalSquare 64