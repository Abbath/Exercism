module DNA where 

import qualified Data.Map as M

count :: Char -> String -> Int
count c _ | not (c `elem` "AGTC") = error $ "invalid nucleotide '"++[c]++"'"
count c s = foldl (\t n -> if n == c then t + 1 else t) 0 . validate $ s
                  

validate :: String -> String
validate s = let a = filter (\n -> not $ (n `elem` "AGTC")) s
             in if null a
                   then s 
                   else error $ "invalid nucleotide '"++[head a]++"'"
                  
nucleotideCounts :: String -> M.Map Char Int                  
nucleotideCounts s = if validate s == s then M.fromList $ map (\c -> (c, count c s)) "AGTC" else undefined