module Minesweeper where

import Data.Char
import Data.List

annotate :: [String] -> [String]
annotate b = let r = length b
                 c = length $ b !! 0
                 bombs = filter (\(x,y)->b !! x !! y == '*') [(x,y)|x <- [0..r-1], y <- [0..c-1]]
             in foldl' mark b bombs

mark :: [String] -> (Int, Int) -> [String]             
mark b (n,m) = map (\x -> map (\y -> f (x,y)) [0..length (b !! 0)-1]) [0..length b-1]
    where 
        f (x,y) | abs(x-n) `elem` [0,1] && abs (y-m) `elem` [0,1] && not ((x,y) == (n,m)) = p (b !! x !! y)
        f (x,y) = b !! x !! y
        p ' ' = '1'
        p '*' = '*'
        p '8' = '8'
        p c | c >= '1' && c <= '7' = chr (ord c + 1)  
        p _ = error "Wrong!"