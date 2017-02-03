module Dominoes (chain) where

import Data.List
import Data.Tree
import Data.Tuple

makeTree :: Eq a => [(a,a)] -> Tree (a,a)
makeTree (s:ss) = unfoldTree (\(x,xs) -> (x, smack (my (snd x) xs) xs)) (s,ss)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain [(a,b)] | a == b = Just [(a,b)]
chain dominoes = let w = makeTree dominoes 
                     n = length $ levels w
                 in if n == length dominoes && fst (rootLabel w) `elem` map snd (levels w !! (n-1))
                       then Just . snd . longestChain 0 (n-1) $ w
                       else Nothing

smack x y = map (\a -> (a, (y \\ [a]) \\ [swap a])) x 

my x y = map (\(a,b) -> if a == x 
                           then (a,b) 
                           else (b,a)) $ filter (\(a,b) -> a == x || b == x) y 

longestChain :: Int -> Int -> Tree (Int,Int) -> (Bool, [(Int,Int)])
longestChain n m (Node a b) = if null b && n /= m 
                                then (False,[]) 
                                else let x = filter fst $ map (longestChain (n+1) m) b
                                     in if null x 
                                           then (True, [a]) 
                                           else (True, a : snd (maximumBy (\x y-> compare (length $ snd x) (length $ snd y)) x))    
