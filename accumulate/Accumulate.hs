module Accumulate where

accumulate :: (t -> t1) -> [t] -> [t1]
accumulate _ [] = []
accumulate f (x:xs) = f x : accumulate f xs