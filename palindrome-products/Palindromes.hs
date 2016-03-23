module Palindromes where

import Data.List 

genericPalindrome :: (Show a, Integral a) => (a -> a -> [a]) -> ([a] -> a) -> a -> a -> (a, [(a,a)])
genericPalindrome f g a b = let p = take 2000000 [x*y| x <- f a b, y <- f a b] -- the dirty hack
                                h = g . filter (\x -> show x == reverse (show x)) $ p
                                s = map (\x -> (x,h `div` x)) . filter (\x -> h `mod` x == 0 && h `div` x `elem` [a..b]) $ [a..b]
                            in (h,s)

smallestPalindrome :: (Show a, Integral a) => a -> a -> (a, [(a,a)])
smallestPalindrome = genericPalindrome (\a b -> [a..b]) minimum

largestPalindrome :: (Show a, Integral a) => a -> a -> (a, [(a,a)])
largestPalindrome = genericPalindrome (\a b -> [b,b-1..a]) maximum