module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just $ primes !! (n - 1)

primes :: [Integer]
primes = map fst $ iterate (uncurry nextPrimes) (2, [2])

nextPrimes :: Integer -> [Integer] -> (Integer, [Integer])
nextPrimes last knownPrimes = (p, knownPrimes ++ [p])
  where p = head $ filter (\x -> not $ any ((0 ==) . mod x) knownPrimes) [(last + 1)..]
