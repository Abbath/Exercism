module Series where

import Data.List
import Data.Char

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n s = zipWith (\x _ -> digits $ take n x) (tails s) (drop (n-1) s)

largestProduct :: Int -> String -> Maybe Int
largestProduct 0 _ = Just 1
largestProduct n s | length s < n = Nothing
largestProduct n s = Just . maximum . map product . slices n $ s 