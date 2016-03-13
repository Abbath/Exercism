module Phone where

import Data.Char

areaCode :: String -> String
areaCode = take 3

number :: String -> String
number = validate . filter isDigit
        
validate :: String -> String        
validate s | length s == 10 = s 
validate s | length s < 10 || length s > 11 = "0000000000"
validate s | length s == 11 && head s == '1' = tail s
validate _ = "0000000000"

prettyPrint :: String -> String
prettyPrint s = let s1 = validate s in "(" ++ areaCode s1 ++ ")" ++ " " ++ (take 3 . drop 3 $ s1) ++ "-" ++ drop 6 s1