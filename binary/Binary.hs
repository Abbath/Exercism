module Binary where

import Data.List

toDecimal :: String -> Int
toDecimal = foldl' convert 0

convert :: Int -> Char -> Int
convert n '1' = n*2+1
convert n '0' = n*2
convert _ _ = 0