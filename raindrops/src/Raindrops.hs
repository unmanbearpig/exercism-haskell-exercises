module Raindrops (convert) where

import Data.Maybe (fromMaybe)

type Sound = (Int, String)

sounds :: [Sound]
sounds = [(3, "Pling"), (5, "Plang"), (7, "Plong")]

matchSound :: Int -> Sound -> Maybe String
matchSound x (m, soundString)
  | x `mod` m == 0 = Just soundString
  | otherwise      = Nothing

matchAllSounds :: [Sound] -> Int -> Maybe String
matchAllSounds sounds x = foldMap (matchSound x) sounds

convert :: Int -> String
convert x = fromMaybe (show x) $ matchAllSounds sounds x
