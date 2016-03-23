{-# LANGUAGE FlexibleInstances #-}
module Connect where 

import Data.List

data Color = Black | White deriving (Show, Eq)

data Start = Begin | End deriving (Show, Eq)

data Tree a = Node a [Tree a] deriving  (Eq)

instance Show (Tree (Int,Int)) where
    show = showTree 0

showTree :: Int -> Tree (Int,Int) -> String
showTree n (Node a s) = show a ++ "\n" ++ replicate n ' ' ++ concatMap (showTree (n+1)) s

elemTree :: Eq a => a -> Tree a -> Bool
elemTree e (Node a []) = if a == e then True else False
elemTree e (Node a s) = if a == e then True else any (e `elemTree`) s 
    
resultFor :: [String] -> Maybe Color
resultFor = check

charToColor :: Char -> Color
charToColor 'X' = Black
charToColor 'O' = White
charToColor _ = error "Bad data!"

check :: [[Char]] -> Maybe Color
check s = if (null iob || null ioe) && (null ixb || null ixe) 
            then Nothing
            else let whb = checkCase White Begin iob ioe
                     whe = checkCase White End ioe iob
                     blb = checkCase Black Begin ixb ixe
                     ble = checkCase Black End ixe ixb
                 in if whb || whe then Just White else if blb || ble then Just Black else Nothing
    where 
        iob = map (\y -> (0,y)) $ elemIndices 'O' (s !! 0)
        ioe = map (\y -> (length s,y)) $ elemIndices 'O' (last s)
        ixb = map (\x -> (x,0)) $ elemIndices 'X' (map head s)
        ixe = map (\x -> (x,length (s!!0))) $ elemIndices 'X' (map last s)
        checkCase clr stt i o = any (\t -> any (`elemTree` t) o) $ map (go clr stt s []) i

search :: Foldable t =>
          Color
          -> [[Char]] -> t (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
search color arr from (cx, cy) = 
    (\x -> if null x then Nothing else Just x) 
    $ map fst 
    $ filter snd 
    $ map (testCell color arr from (cx, cy)) 
    $ [(x,y)| x <- [cx-1,cx,cx+1], y <- [cy-1, cy, cy+1], x >= 0, x < length arr, y >= 0, y < length (arr!!0)]

testCell :: Foldable t =>
            Color
            -> [[Char]]
            -> t (Int, Int)
            -> (Int, Int)
            -> (Int, Int)
            -> ((Int, Int), Bool)
testCell color arr from (cx, cy) (x,y)
    |x == cx && y == cy = ((x,y),False)
    |cx - x == 1 && cy - y == 1 = ((x,y),False)
    |x - cx == 1 && y - cy == 1 = ((x,y),False)
    |(x,y) `elem` from = ((x,y),False) 
    |arr !! x !! y /= '.' && color == charToColor (arr !! x !! y) = ((x,y),True)
    |otherwise = ((x,y),False) 

go :: Color
      -> Start
      -> [[Char]]
      -> [(Int, Int)]
      -> (Int, Int)
      -> Tree (Int, Int)
go c s arr from (x,y) 
    |(c,s) == (White, Begin) && x == length arr - 1 = Node (x,y) []
    |(c,s) == (White, End) && x == 0 = Node (x,y) []
    |(c,s) == (Black, Begin) && y == length (arr !! 0) -1 = Node (x,y) []
    |(c,s) == (Black, End) && y == 0 = Node (x,y) []
    |otherwise = let f = search c arr from (x,y)
                 in case f of 
                         Nothing -> Node (x,y) []
                         Just r -> Node (x,y) $ map (go c s arr ((x,y):from)) r
                         