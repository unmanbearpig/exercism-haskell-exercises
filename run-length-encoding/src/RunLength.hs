module RunLength (decode, encode) where
import Data.List (group, groupBy)
import Data.Char (isDigit)

data StringWithCount = StringWithCount Int String deriving (Show)

deflateGroup (x:[]) = [x]
deflateGroup group = (show $ length group) ++ [head group]

encode :: String -> String
encode [] = []
encode x = concat $ map deflateGroup $ group x

extractCounts :: [String] -> [StringWithCount]
extractCounts [] = []
extractCounts (gx:[]) = [(StringWithCount 1 gx)]
extractCounts (gx:gy:gs) =
  if isDigit $ head gx
  then [StringWithCount (read gx) gy] ++ extractCounts gs
  else [(StringWithCount 1 gx)] ++ extractCounts (gy:gs)

-- It's not very nice of me to assume we would get a single-character string here, I should fix it probably and replace it with char
compileNumStringPair :: StringWithCount -> String
compileNumStringPair (StringWithCount count string) = replicate count (head string)

groupDigits = groupBy (\x y -> isDigit x && isDigit y)

decode :: String -> String
decode = concat . map compileNumStringPair . extractCounts . groupDigits
