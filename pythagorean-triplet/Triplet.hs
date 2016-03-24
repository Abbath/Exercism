module Triplet where

import Data.List

mkTriplet a b c = [a,b,c]

isPythagorean x = let [a,b,c] = sort x 
                  in a^2 + b^2 == c^2

pythagoreanTriplets l u = filter isPythagorean 
    $ [[a,b,c]| a <- [l..u], b <- [a..u], c <- [b..u]]