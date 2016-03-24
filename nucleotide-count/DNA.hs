module DNA where 

import qualified Data.Map as M

count :: Char -> String -> Int
count c _ | isNotOk c = error $ "invalid nucleotide '"++[c]++"'"
count c s = foldl (\t n -> if n == c then t + 1 else t) 0 . validate $ s
                  
isNotOk :: Char -> Bool
isNotOk = not . (`elem` "AGTC")
                  
validate :: String -> String
validate s = let a = filter isNotOk s
             in if null a
                   then s 
                   else error $ "invalid nucleotide '"++[head a]++"'"
                  
nucleotideCounts :: String -> M.Map Char Int                  
nucleotideCounts s = validate s `seq` M.fromList $ map (\c -> (c, count c s)) "AGTC"