module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Text as T

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultNames = [ "Alice"
               , "Bob"
               , "Charlie"
               , "David"
               , "Eve"
               , "Fred"
               , "Ginny"
               , "Harriet"
               , "Ileana"
               , "Joseph"
               , "Kincaid"
               , "Larry"  ]

defaultGarden :: String -> Map.Map String [Plant]
defaultGarden = garden defaultNames

parsePlantString :: String -> [[Plant]]
parsePlantString = map (map parsePlant) . groupPlantChars

groupPlantChars :: String -> [String]
groupPlantChars = map T.unpack . zipRows . map (T.chunksOf 2) . T.lines . T.pack

zipRows :: [[T.Text]] -> [T.Text]
zipRows (r1:r2:[]) = zipWith T.append r1 r2
zipRows _ = error "have to have two rows"

parsePlant :: Char -> Plant
parsePlant x
  | x == 'V' = Violets
  | x == 'R' = Radishes
  | x == 'G' = Grass
  | x == 'C' = Clover
  | otherwise = error "argh"

garden :: [String] -> String -> Map.Map String [Plant]
garden names serializedRows = Map.fromList $ zip (sort names) (parsePlantString serializedRows)

lookupPlants :: String -> Map.Map String [Plant] -> [Plant]
lookupPlants = Map.findWithDefault []
