module Beer where

import Data.List

verse :: Int -> String
verse n = 
    (if n == 0 then "No more" else show n) ++
    " bottle" ++
    (if n == 1 then "" else "s") ++
    " of beer on the wall, " ++
    (if n == 0 then "no more" else show n) ++
    " bottle" ++
    (if n == 1 then "" else "s") ++
    " of beer.\n" ++
    (if n == 0
       then "Go to the store and buy some more, " 
       else "Take " ++
            (if n == 1 then "it" else "one") ++
            " down and pass it around, ") ++
    (case n of 
         0 -> "99"
         1 -> "no more"
         _ -> show (n-1)) ++
    " bottle" ++
    (if n == 2 then "" else "s") ++
    " of beer on the wall.\n"
    
sing :: Int -> Int -> [Char]
sing a b = (++"\n") . intercalate "\n" $ map verse [a,a-1..b]