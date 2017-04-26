module Brackets (arePaired) where

import Data.List (find)
import Data.Maybe (catMaybes)

data BracketKind = BracketKind { open :: Char
                               , close :: Char }
  deriving (Eq, Show)

data BracketValue = Open | Close deriving (Eq, Show)

data Bracket = Bracket { value :: BracketValue
                       , kind :: BracketKind }
  deriving (Eq)


defaultBrackets = [ (BracketKind '(' ')')
                  , (BracketKind '[' ']')
                  , (BracketKind '{' '}') ]

instance Show Bracket where
  show (Bracket value (BracketKind a b))
    | value == Open = [a]
    | otherwise     = [b]

bChars :: BracketKind -> [Char]
bChars (BracketKind a b) = [a, b]

findBracket :: [BracketKind] -> Char -> Maybe Bracket
findBracket bks c =
  do kind <- find (elem c . bChars) bks
     value <- return $ if open kind == c then Open else Close
     return $ Bracket value kind

isOpen :: Bracket -> Bool
isOpen b = Open == value b

isClose :: Bracket -> Bool
isClose b = Close == value b

isPair :: Bracket -> Bracket -> Bool
isPair a b
  | kind a /= kind b = False
  | isOpen a && isClose b = True
  | otherwise = False

collapseBy :: (a -> a -> Bool) -> [a] -> [a]
collapseBy _ []  = []
collapseBy _ [a] = [a]
collapseBy f (a:b:xs) = if f a b then collapseBy f xs else a:collapseBy f (b:xs)

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f a = if f a == a then a else fixPoint f $ f a

arePaired :: String -> Bool
arePaired = null
          . fixPoint (collapseBy isPair)
          . catMaybes
          . map (findBracket defaultBrackets)
