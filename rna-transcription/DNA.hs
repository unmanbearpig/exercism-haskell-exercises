module DNA (toRNA) where

transcribe :: Char -> Char
transcribe c
  | c == 'C' = 'G'
  | c == 'G' = 'C'
  | c == 'A' = 'U'
  | c == 'T' = 'A'
  | otherwise = c

toRNA :: String -> String
toRNA = map transcribe