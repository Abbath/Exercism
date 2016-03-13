module LinkedList where

data List a = Cons a (List a) | Nil deriving (Show, Eq)

nil :: List a
nil = Nil

new :: a -> List a -> List a
new = Cons

isNil :: List a -> Bool
isNil Nil = True
isNil _ = False

toList :: List a -> [a]
toList Nil = []
toList (Cons a l) = a : toList l

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

datum :: List a -> a
datum Nil = error "Empty list!"
datum (Cons a _) = a

next :: List a -> List a
next Nil = error "Empty list!"
next (Cons _ l) = l

reverseLinkedList :: List a -> List a
reverseLinkedList Nil = Nil
reverseLinkedList s = r Nil s 
    where 
        r acc Nil = acc
        r acc (Cons a l) = r (Cons a acc) l   