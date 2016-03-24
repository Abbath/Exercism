module Series where

import Data.List
import Data.Char

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n s = zipWith (\x _ -> digits $ take n x) (tails s) (drop (n-1) s)