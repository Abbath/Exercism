module Atbash where

import Data.List
import Data.Char

encode :: String -> String
encode = 
    intercalate " " 
    . unfoldr (\x -> if x /= [] then Just (take 5 x, drop 5 x) else Nothing ) 
    . map (\x -> if isAlpha x then chr (122 - (ord (toLower x) - 97)) else x)
    . filter (\x -> isAlphaNum x && isAscii x) 