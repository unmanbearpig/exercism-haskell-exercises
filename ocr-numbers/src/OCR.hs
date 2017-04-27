module OCR (convert) where

import Data.List (find, intercalate)
import Data.List.Split (chunksOf)

newtype Chunk = Chunk [String] deriving (Eq)
data Character = Character { value :: String, charChunk :: Chunk }

chars = [ Character "0" ( Chunk [ " _ "
                                , "| |"
                                , "|_|"
                                , "   " ])
        , Character "1" ( Chunk [ "   "
                                , "  |"
                                , "  |"
                                , "   " ])
        , Character "2" ( Chunk [ " _ "
                                , " _|"
                                , "|_ "
                                , "   " ])
        , Character "3" ( Chunk [ " _ "
                                , " _|"
                                , " _|"
                                , "   " ])
        , Character "4" ( Chunk [ "   "
                                , "|_|"
                                , "  |"
                                , "   " ])
        , Character "5" ( Chunk [ " _ "
                                , "|_ "
                                , " _|"
                                , "   " ])
        , Character "6" ( Chunk [ " _ "
                                , "|_ "
                                , "|_|"
                                , "   " ])
        , Character "7" ( Chunk [ " _ "
                                , "  |"
                                , "  |"
                                , "   " ])
        , Character "8" ( Chunk [ " _ "
                                , "|_|"
                                , "|_|"
                                , "   " ])
        , Character "9" ( Chunk [ " _ "
                                , "|_|"
                                , " _|"
                                , "   " ]) ]


matchChar :: [Character] -> Chunk -> Maybe Character
matchChar cs chunk = find (\c -> chunk == charChunk c) cs

splitCols :: Int -> [String] -> [Chunk]
splitCols w ls
  | lWidth < w = []
  | otherwise = Chunk (take w <$> ls):splitCols w (drop w <$> ls)
  where lWidth = length $ head ls

convert :: String -> String
convert s = intercalate "," $ concat <$> fmap (maybe "?" value . matchChar chars) <$> splitCols 3 <$> chunksOf 4 (lines s)
