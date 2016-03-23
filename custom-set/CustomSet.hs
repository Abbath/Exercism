module CustomSet where

import Data.List

newtype CustomSet a = CustomSet [a] deriving (Eq)

instance Show a => Show (CustomSet a) where
    show (CustomSet a) = "fromList " ++ show a

fromList :: Ord a => [a] -> CustomSet a
fromList = CustomSet . sort . nub 

empty :: CustomSet a
empty = CustomSet []

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet s) = CustomSet $ filter (/= x) s

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet a) (CustomSet b) = CustomSet $ (\\) a b

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom a b = intersection a b == empty

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet a) (CustomSet b) = CustomSet $ intersect a b

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert a (CustomSet l) = fromList $ Data.List.insert a l

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet a) (CustomSet b) = isInfixOf a b

toList :: CustomSet a -> [a]
toList (CustomSet a) = a

size :: CustomSet a -> Int
size (CustomSet a) = length a

null :: CustomSet a -> Bool
null (CustomSet a) = Data.List.null a

member :: Eq a => a -> CustomSet a -> Bool
member a (CustomSet b) = a `elem` b

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet a) (CustomSet b) = fromList $ a ++ b 
