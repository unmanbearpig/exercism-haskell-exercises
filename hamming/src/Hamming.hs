module Hamming (distance) where

isSameLength x y = length x == length y

validDistance :: String -> String -> Int
validDistance xs ys = length $ filter (\x -> x == False) $ map (uncurry (==)) $ zip xs ys

distance :: String -> String -> Maybe Int
distance xs ys = if isSameLength xs ys
                 then Just (validDistance xs ys)
                 else Nothing
