{-# LANGUAGE BangPatterns #-}
module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f !z [] = z
foldl' f !z (x:xs) = foldl' f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)   

length :: [a] -> Int
length = foldl' (\x _-> x + 1) 0  

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x l -> f x : l) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x l -> if f x then x : l else l) []

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys) 

concat :: [[a]] -> [a]
concat = foldr (++) []
