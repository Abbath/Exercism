module Sieve where 

import Data.List

primesUpTo :: (Enum a, Eq a, Num a) => a -> [a]
primesUpTo m = sieve [2..m]
    where 
        sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
        sieve [] = []