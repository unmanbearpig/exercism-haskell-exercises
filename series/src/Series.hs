module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices 0 _  = [[]]
slices n xs = map (\m -> take n $ drop m nums) [0..(length nums - n)]
  where nums = map digitToInt xs :: [Int]
