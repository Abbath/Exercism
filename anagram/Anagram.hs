module Anagram where 

import Data.Char
import Data.List

anagramsFor :: String -> [String] -> [String]
anagramsFor x s = let xu = map toUpper x
                      sx = sort xu
                  in filter (\y -> let yu = map toUpper y 
                                   in xu /= yu && sx == sort yu) s  