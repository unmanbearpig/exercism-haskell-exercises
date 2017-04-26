module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq)



allergyScores =  [ (1, Eggs)
                 , (2, Peanuts)
                 , (4, Shellfish)
                 , (8, Strawberries)
                 , (16, Tomatoes)
                 , (32, Chocolate)
                 , (64, Pollen)
                 , (128, Cats) ]


allergies :: Int -> [Allergen]
allergies score = map snd $ filter (\(s, a) -> score .&. s > 0 ) allergyScores

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = elem allergen $ allergies score
