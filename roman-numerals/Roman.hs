module Roman where

import Data.Maybe

template :: String -> Int -> [(Int, String)]
template [a,b,c] n = [(1*n,[a]), (2*n,[a,a]), (3*n,[a,a,a]), (4*n,[a,b]), (5*n,[b]), (6*n,[b,a]), (7*n,[b,a,a]), (8*n,[b,a,a,a]), (9*n,[a,c])] 
template _ _ = error ""

basics :: [(Int, [Char])]
basics = template "IVX" 1 ++ template "XLC" 10 ++ template "CDM" 100
      
numerals :: Int -> String
numerals n | n > 3999 = error "Too big!"
numerals n = concat (replicate (n `div` 1000) "M") ++ l (n1 `div` 100 * 100) ++ l (n2 `div` 10 * 10) ++ l n3
    where 
        l x = fromMaybe "" . lookup x $ basics
        n1 = n `mod` 1000
        n2 = n1 `mod` 100
        n3 = n2 `mod` 10