module Matrix (saddlePoints) where

import Data.Array
import Data.List

type Index = (Int, Int)

arr :: Array Index Int
arr = listArray ((0,0), (2, 4)) $ concat [ [18,  3, 39, 19,  91]
                                         , [38, 10,  8, 77, 320]
                                         , [ 3,  4,  8,  6,   7] ]

saddlePoints :: Ord e => Array Index e -> [Index]
saddlePoints matrix = map fst $ intersect (rowMaxes matrix) (colMins matrix)

rowMaxes :: Ord e => Array Index e -> [(Index, e)]
rowMaxes = concat . map (maxesOn snd) . rows

colMins :: Ord e => Array Index e -> [(Index, e)]
colMins = concat . map (minsOn snd) . columns

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy $ \x y -> (f x) == (f y)

rows :: Array Index e -> [[(Index, e)]]
rows = groupOn f . sortOn f . assocs
  where f = (fst . fst)

columns :: Array Index e -> [[(Index, e)]]
columns = groupOn f . sortOn f . assocs
  where f = (snd . fst)

maxesOn :: (Ord b, Eq a) => (a -> b) -> [a] -> [a]
maxesOn f = head . groupOn f . reverse . sortOn f

minsOn :: (Ord b, Eq a) => (a -> b) -> [a] -> [a]
minsOn f = head . groupOn f . sortOn f
