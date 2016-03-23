module Series where

import Data.List
import Data.Char

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n s = map (take n) . filter ((>=n) . length) . tails $ digits s 