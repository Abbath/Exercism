module Series where

import Data.List
import Data.Char

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n s = map (take n) . filter ((>=n) . length) . tails $ digits s 

largestProduct :: Int -> String -> Maybe Int
largestProduct 0 _ = Just 1
largestProduct n s | length s < n = Nothing
largestProduct n s = Just . maximum . map product . slices n $ s 