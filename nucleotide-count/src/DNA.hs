module DNA (nucleotideCounts) where

import qualified Data.Map as Map

initialMap = Map.fromList [ ('A', 0)
                          , ('C', 0)
                          , ('G', 0)
                          , ('T', 0) ]
charsWithCount :: String -> [(Char, Int)]
charsWithCount = map (\c -> (c, 1))

isStrandValid :: String -> Bool
isStrandValid = all (\x -> Map.member x initialMap)

countNucleotides :: String -> Map.Map Char Int
countNucleotides = Map.unionWith (+) initialMap . Map.fromListWith (+) . charsWithCount

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts xs
  | isStrandValid xs = Right $ countNucleotides xs
  | otherwise = Left xs
