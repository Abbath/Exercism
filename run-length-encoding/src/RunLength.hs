module RunLength (decode, encode) where

import Data.List
import Data.Char

decode :: String -> String
decode = go 
    where
        go [] = []
        go s = let c = takeWhile isDigit s
                   n = if null c then 1 else read (takeWhile isDigit s) :: Int 
                   m = dropWhile isDigit s
               in replicate n (head m) ++ go (tail m)   
          
encode :: String -> String
encode = concatMap (\x -> let n = length x in (if n == 1 then "" else show (length x)) ++ [head x]) . group 
