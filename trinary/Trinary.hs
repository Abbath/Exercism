{-# LANGUAGE BangPatterns #-}
module Trinary where

import qualified Data.Map as M

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !z [] = z
foldl' f !z (x:xs) = foldl' f (f z x) xs

readTri :: (Integral a) => String -> a
readTri = foldl' convert 0

showTri :: (Integral a) => a -> String
showTri n = reverse . s $ n 
    where 
        s 0 = []
        s m = chars M.! (m `mod` 3) : s (m `div` 3)

digits :: (Integral a) =>  M.Map Char a 
digits = M.fromList [('0', 0),('1', 1),('2', 2)]

chars :: (Integral a) => M.Map a Char 
chars = M.fromList [(0, '0'),(1, '1'),(2, '2')]

convert :: (Integral a) => a -> Char -> a
convert n d = case M.lookup d digits of
                   Just x ->  n*3+x 
                   Nothing -> 0