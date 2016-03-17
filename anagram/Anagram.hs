module Anagram where 

import Data.Char
import Data.List

anagramsFor :: [Char] -> [[Char]] -> [[Char]]
anagramsFor x s = let xu = map toUpper $ x
                      sx = sort xu
                  in filter (\y -> let yu = map toUpper y in sx == sort yu && xu /= yu) s  