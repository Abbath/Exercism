{-# LANGUAGE BangPatterns #-}
module Trinary where

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !z [] = z
foldl' f !z (x:xs) = foldl' f (f z x) xs

readTri :: (Integral a) => String -> a
readTri = foldl' convert 0

showTri :: (Integral a) => a -> String
showTri n = reverse . s $ n 
    where 
        s 0 = []
        s m = fromDigit (m `mod` 3) : s (m `div` 3)

toDigit :: Integral a => Char -> Maybe a
toDigit c = let t = fromEnum c - fromEnum '0' 
            in if t >= 0 && t <= 2 
                  then Just $ fromIntegral t 
                  else Nothing 
                
fromDigit :: Integral a => a -> Char
fromDigit n = toEnum (fromIntegral n + fromEnum '0')

convert :: (Integral a) => a -> Char -> a
convert n d = case toDigit d of
                   Just x ->  n*3+x 
                   Nothing -> 0