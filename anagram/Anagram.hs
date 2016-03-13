module Anagram where 

import Data.Char
import Data.List

anagramsFor :: [Char] -> [[Char]] -> [[Char]]
anagramsFor x s = filter (\y -> let (xu, yu, tu, sr) = (tu x, tu y, map toUpper, sort) in sr xu == sr yu && xu /= yu) s