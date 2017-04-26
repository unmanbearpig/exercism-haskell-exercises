module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Empty | Node a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum Empty = error "this should be a Maybe, right?"
datum (Node x _) = x

fromList :: [a] -> LinkedList a
fromList []     = Empty
fromList (x:xs) = Node x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _     = False

new :: a -> LinkedList a -> LinkedList a
new a list = Node a list

next :: LinkedList a -> LinkedList a
next Empty = Empty
next (Node _ rest) = rest

nil :: LinkedList a
nil = Empty

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList Empty = Empty
reverseLinkedList a = reverseLinkedList' a Empty

reverseLinkedList' :: LinkedList a -> LinkedList a -> LinkedList a
reverseLinkedList' Empty to = to
reverseLinkedList' (Node a rest) to = reverseLinkedList' rest (new a to)

toList :: LinkedList a -> [a]
toList Empty = []
toList (Node a rest) = a:(toList rest)
