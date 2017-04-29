{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CustomSet
  ( delete
  , difference
  , Control.Applicative.empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null, filter)
import Control.Applicative
import Data.List (foldl')
import qualified Data.Vector as V

newtype CustomSet a = CustomSet { vect :: V.Vector a }
  deriving (Show, Functor, Foldable, Applicative, Alternative)

-- I'm not sure if I'm supposed to add Ord constraint and implement less inefficiently
instance Eq a => Eq (CustomSet a) where
  a == b = a `isSubsetOf` b && b `isSubsetOf` a

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x = filter (/= x)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = filter (\a -> not $ member a setB) setA

fromList :: Eq a => [a] -> CustomSet a
fromList = foldl' (flip insert) empty

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set
  | x `member` set = set
  | otherwise = CustomSet $ V.cons x $ vect set

filter :: Eq a => (a -> Bool) -> CustomSet a -> CustomSet a
filter f set = CustomSet $ V.filter f $ vect set

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = filter (`member` setB) setA

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = not $ any (`elem` setB) setA

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB =  alen <= blen && all (`elem` setB) setA
  where alen = V.length $ vect setA
        blen = V.length $ vect setB

member :: Eq a => a -> CustomSet a -> Bool
member = elem

null :: CustomSet a -> Bool
null = V.null . vect

size :: CustomSet a -> Int
size = V.length . vect

toList :: CustomSet a -> [a]
toList = V.toList . vect

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = CustomSet $ vect setA V.++ V.filter (\a -> not $ member a setA) (vect setB)
