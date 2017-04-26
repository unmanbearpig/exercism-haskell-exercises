module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort, sortOn)

data School = School (Map.Map Int [String])

mapSchool f (School smap) = School (f smap)

add :: Int -> String -> School -> School
add gradeNum student = mapSchool $ Map.insertWith (++) gradeNum [student]

empty :: School
empty = School (Map.fromList [])

unwrap :: Maybe [a] -> [a]
unwrap (Just a) = a
unwrap Nothing  = []

grade :: Int -> School -> [String]
grade gradeNum (School smap) = unwrap $ fmap sort $ Map.lookup gradeNum smap

gradeIds :: School -> [Int]
gradeIds (School smap) = Map.keys smap

sorted :: School -> [(Int, [String])]
sorted school = map (\gradeId -> (gradeId, grade gradeId school)) $ sort $ gradeIds school
