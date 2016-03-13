module Hexadecimal where

import qualified Data.Map as M

hexToInt :: String -> Int
hexToInt = foldl convert 0

digits :: M.Map Char Int 
digits = M.fromList [('0', 0),('1', 1),('2', 2),('3', 3),('4', 4),('5', 5),('6', 6),('7', 7),('8', 8),('9', 9),('a', 10),('b', 11),('c', 12),('d', 13),('e', 14),('f', 15)]

convert :: Int -> Char -> Int
convert n d = case M.lookup d digits of
                   Just x ->  n*16+x 
                   Nothing -> 0