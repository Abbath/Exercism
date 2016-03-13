module Bob where 

import Data.Char

responseFor :: String -> String
responseFor s 
    | all isSpace s = "Fine. Be that way!"
    | (\x -> not (null x) && all isUpper x) . filter isAlpha $ s = "Whoa, chill out!"
    | last s == '?' = "Sure."
    | otherwise = "Whatever."