module Scrabble (scoreLetter, scoreWord) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

letterScores = [(1, "aeioulnrst"),
                (2, "dg"),
                (3, "bcmp"),
                (4, "fhvwy"),
                (5, "k"),
                (8, "jx"),
                (10, "qz")]

letterScoreMap = foldl (\acc (k, letters) -> foldl
                         (\acc letter -> Map.insert letter k acc)
                         acc letters)
                 Map.empty letterScores

scoreLetter :: Char -> Int
scoreLetter letter = fromMaybe 0 $ Map.lookup (toLower letter) letterScoreMap

scoreWord :: String -> Int
scoreWord = foldl (+) 0 . map scoreLetter
