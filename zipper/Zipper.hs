module Zipper (
    BinTree(..),
    Zipper,

    fromTree,
    toTree,

    value,
    left,
    right,
    up,

    setValue,
    setLeft,
    setRight
) where

-- | A binary tree.
data BinTree a = BT { 
    btValue :: a                 -- ^ Value
  , btLeft  :: Maybe (BinTree a) -- ^ Left child
  , btRight :: Maybe (BinTree a) -- ^ Right child
} deriving (Eq, Show)

data Direction = L | R deriving (Show)  
data Crumb a = LeftCrumb a (Maybe (BinTree a)) | RightCrumb a (Maybe (BinTree a)) deriving (Show, Eq)
type Breadcrumbs a = [Crumb a]  

-- | A zipper for a binary tree.
data Zipper a = Zipper (BinTree a) (Breadcrumbs a) deriving (Eq, Show)-- Complete this definition

-- | Get a zipper focussed on the root node.
fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

-- | Get the complete tree from a zipper.
toTree :: Zipper a -> BinTree a
--toTree (Zipper t []) = t
toTree z@(Zipper t _) = case up z of
                Nothing -> t
                Just w -> toTree w 
                              
-- | Get the value of the focus node.
value :: Zipper a -> a
value (Zipper t _) = btValue t 

-- | Get the left child of the focus node, if any.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper t d) = case btLeft t of 
                         Nothing -> Nothing
                         Just l -> Just $ Zipper l (LeftCrumb (btValue t) (btRight t):d)

-- | Get the right child of the focus node, if any.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper t d) = case btRight t of 
                         Nothing -> Nothing
                         Just r -> Just $ Zipper r (RightCrumb (btValue t) (btLeft t):d)
            
-- | Get the parent of the focus node, if any.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper t (LeftCrumb x r : bs)) = Just $ Zipper (BT x (Just t) r) bs
up (Zipper t (RightCrumb x l : bs)) = Just $ Zipper (BT x l (Just t)) bs
    
-- | Set the value of the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper t bs) = Zipper (t{btValue = x}) bs

-- | Replace a left child tree.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft b (Zipper t bs) = Zipper (t{btLeft = b}) bs

-- | Replace a right child tree.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight b (Zipper t bs) = Zipper (t{btRight = b}) bs 
