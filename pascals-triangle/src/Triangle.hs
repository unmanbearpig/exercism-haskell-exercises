module Triangle (rows) where

nextLevel :: [Integer] -> [Integer]
nextLevel xs = zipWith (+) (0:xs) (xs ++ [0])

rows :: Int -> [[Integer]]
rows n = take n $ iterate nextLevel [1]
