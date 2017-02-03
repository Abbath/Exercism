module Hamming (distance) where

distance :: Eq a => [a] -> [a] -> Maybe Int
distance a b | length a /= length b = Nothing
distance a b = Just . length . filter id $ zipWith (/=) a b 
