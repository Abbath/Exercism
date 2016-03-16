{-# LANGUAGE BangPatterns #-}
module Octal where

import qualified Data.Map as M

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !z [] = z
foldl' f !z (x:xs) = foldl' f (f z x) xs

readOct :: (Integral a) => String -> a
readOct = foldl' convert 0

showOct :: (Integral a) => a -> String
showOct n = reverse . s $ n 
    where 
        s 0 = []
        s m = chars M.! (m `mod` 8) : s (m `div` 8)

digits :: (Integral a) =>  M.Map Char a 
digits = M.fromList [('0', 0),('1', 1),('2', 2),('3', 3),('4', 4),('5', 5),('6', 6),('7', 7)]

chars :: (Integral a) => M.Map a Char 
chars = M.fromList [(0, '0'),(1, '1'),(2, '2'),(3, '3'),(4, '4'),(5, '5'),(6, '6'),(7, '7')]

convert :: (Integral a) => a -> Char -> a
convert n d = case M.lookup d digits of
                   Just x ->  n*8+x 
                   Nothing -> 0