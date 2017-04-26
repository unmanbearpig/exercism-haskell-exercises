module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)
import Data.List (transpose)
import Data.List.Split (chunksOf)

gridChunkSizes :: Int -> (Int, Int) -> [Int]
gridChunkSizes s (r, c) = replicate (r - shortRows) c
                       ++ replicate shortRows (c - 1)
  where shortRows = r * c - s

gridDimensions :: Int -> (Int, Int)
gridDimensions len = (r, c)
  where c = ceiling sqr
        r = if fl * c < len then fl + 1 else fl
        fl = floor sqr
        sqr = sqrt $ fromIntegral len

chunkString :: [Int] -> String -> [String]
chunkString  _ [] = []
chunkString [] _  = error "chunkString got not enough chunks for given string"
chunkString (cs:css) s = chunk : chunkString css rest
  where (chunk, rest) = splitAt cs s

formatOutputGrid :: (Int, Int) -> String -> [String]
formatOutputGrid dimensions xs = chunkString chunkSizes xs
  where chunkSizes = gridChunkSizes (length xs) dimensions

encode :: String -> String
encode xs = unwords $ formatOutputGrid (c, r) $ concat $ transpose $ chunksOf c normalized
  where normalized = map toLower $ filter isAlphaNum xs
        nLen = length normalized
        (r, c) = gridDimensions nLen
