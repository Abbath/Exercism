module DNA where

hammingDistance :: Eq a =>  [a] -> [a] -> Int
hammingDistance a b = length . filter (==True) $ zipWith (/=) a b 
