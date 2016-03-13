module Sublist where

import Data.List

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Show, Eq)

sublist a b | a == b = Equal
            | f a b = Sublist 
            | f b a = Superlist
            | otherwise = Unequal
    where f x y = any (pref x) $ y : tails y
          pref [] _  =  True
          pref _  [] =  False
          pref (x:xs) (y:ys) = x == y && pref xs ys                                      