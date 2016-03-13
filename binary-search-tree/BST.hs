module BST where

data BST a = BST a (BST a) (BST a) | Empty deriving (Show, Eq)

bstLeft :: BST t -> Maybe (BST t)
bstLeft Empty = Nothing
bstLeft (BST _ l _) = Just l

bstRight :: BST t -> Maybe (BST t)
bstRight Empty = Nothing
bstRight (BST _ _ r) = Just r

bstValue :: BST t -> t
bstValue Empty = error "Empty tree!"
bstValue (BST v _ _) = v

singleton :: a -> BST a
singleton x = BST x Empty Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (BST v l r) 
    | x <= v = BST v (insert x l) r
    | x > v = BST v l (insert x r)
    | otherwise = error "Magic!"

fromList :: (Ord a) => [a] -> BST a
fromList = foldl (flip insert) Empty

toList :: BST t -> [t]
toList Empty = []
toList (BST v l r) = toList l ++ [v] ++ toList r
