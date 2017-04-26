module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.List (any)
import qualified Data.Vector as V

data Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix rs)
  | V.null rs = 0
  | otherwise = length $ V.head rs

column :: Int -> Matrix a -> V.Vector a
column x (Matrix rs) = fmap (\r -> r V.! x) rs

flatten :: Matrix a -> V.Vector a
flatten matrix@(Matrix rs) = V.generate (width * height) (\i -> (rs V.! (i `div` width)) V.! (i `mod` width))
  where width = cols matrix
        height = rows matrix

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList $ map (V.fromList) xss


reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (width, height) matrix = Matrix $ V.generate height rowSlice
  where flat = flatten matrix
        rowSlice rowNum = V.slice (rowNum * width) width flat

row :: Int -> Matrix a -> V.Vector a
row x (Matrix rs) = rs V.! x

rows :: Matrix a -> Int
rows (Matrix rs) = V.length rs

shape :: Matrix a -> (Int, Int)
shape matrix = ((rows matrix), (cols matrix))

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix $ V.generate (cols matrix) (\ri -> V.fromList $ V.toList $ column ri matrix)

fromString :: Read a => String -> Matrix a
fromString = fromList . map readAll . lines
  where
    readAll s = case reads s of
      [] -> []
      (x, rest):_ -> x : readAll rest
