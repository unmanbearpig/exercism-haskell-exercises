module ETL (transform) where

import Data.Map (Map, insert, foldWithKey, fromList)
import Data.Char (toLower)

newTuple :: a -> Char -> (Char, a)
newTuple k char = ((toLower char), k)

newTuples :: a -> [Char] -> [(Char, a)]
newTuples = map . newTuple

oldMapToTuples :: Map a [Char] -> [(Char, a)]
oldMapToTuples = concat . foldWithKey (\k -> (:) . newTuples k) []

transform :: Map a [Char] -> Map Char a
transform = fromList . oldMapToTuples
