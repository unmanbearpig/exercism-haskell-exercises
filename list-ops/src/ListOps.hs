{-# LANGUAGE BangPatterns #-}

module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

-- Is strict required here for performance reasons?
-- So that Haskell doesn't build too many thunks while trying to evaluate it, right?
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f !acc (x:xs) = foldl' f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x:xs) = f x $ foldr f acc xs

length :: [a] -> Int
length = foldr (\_ acc -> acc + 1) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
