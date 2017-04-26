module Roman (numerals) where

numerals 0 = Nothing
numerals x = Just $ concat $ numerals' x

romanValues = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"),
                 (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"),
                 (5, "V"), (4, "IV"), (1, "I")]

-- refactor me
numerals' :: Int -> [String]
numerals' 0 = []
numerals' x = (replicate q s) ++ numerals' r
  where ((q, r), s) = head $ filter (\((q, r), s) -> q > 0) $
              map (\(v, s) -> ((x `quotRem` v), s)) romanValues
