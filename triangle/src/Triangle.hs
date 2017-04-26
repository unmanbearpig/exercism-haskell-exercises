module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

-- is it possible to generate that list of permutations and pass it into a function
--   instead of listing them here?
forPermutations :: (a -> a -> a -> b) -> (b -> b -> b) -> a -> a -> a -> b
forPermutations f join a b c = foldl join (f a b c)
                               [(f a c b),  (f b a c),  (f b c a),  (f c a b),  (f c b a)]

forPermutationsOfTwo :: (a -> a -> b) -> (b -> b -> b) -> a -> a -> a -> b
forPermutationsOfTwo f join a b c = foldl join (f a b) [(f a c), (f b c)]

triangleType :: (Eq a, Ord a, Fractional a) => a -> a -> a -> TriangleType
triangleType 0 0 0 = Illegal
triangleType z y x
  | forPermutations (\x y z -> x > (y + z)) (||) z y x = Illegal
  | forPermutationsOfTwo (==) (&&) x y z = Equilateral
  | forPermutationsOfTwo (==) (||) x y z = Isosceles
  | otherwise   = Scalene
