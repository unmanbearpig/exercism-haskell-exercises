module Base (rebase) where

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Nothing
  | outputBase < 2 = Nothing
  | any (< 0) inputDigits = Nothing
  | any (>= inputBase) inputDigits = Nothing
  | null inputDigits = Just []
  | otherwise = Just $ toDigits outputBase $ digitsToNativeNum inputBase inputDigits

withIndexes :: Integral b => [a] -> [(a, b)]
withIndexes xs = reverse $ zip (reverse xs) [0..]

digitsToNativeNum :: Num a => a -> [a] -> a
digitsToNativeNum base = sum . map (\(x, i) -> x * (base ^ i)) . withIndexes

toDigits :: Integral a => a -> a -> [a]
toDigits _ 0 = []
toDigits base x = toDigits base (x `div` base) ++ pure (x `mod` base)
