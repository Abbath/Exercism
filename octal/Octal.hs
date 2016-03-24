{-# LANGUAGE BangPatterns #-}
module Octal where

import Data.Char

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !z [] = z
foldl' f !z (x:xs) = foldl' f (f z x) xs

readOct :: (Integral a) => String -> a
readOct = foldl' convert 0

showOct :: (Integral a) => a -> String
showOct n = reverse . s $ n 
    where 
        s 0 = []
        s m = fromDigit (m `mod` 8) : s (m `div` 8)

toDigit :: Integral a => Char -> Maybe a
toDigit c = let t = ord c - ord '0' 
            in if t >= 0 && t <= 7 
                  then Just $ fromIntegral t 
                  else Nothing 
                
fromDigit :: Integral a => a -> Char
fromDigit n = chr (fromIntegral n + ord '0')

convert :: (Integral a) => a -> Char -> a
convert n d = case toDigit d of
                   Just x ->  n*8+x 
                   Nothing -> 0