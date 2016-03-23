module Say where 

import Data.List (intercalate, unfoldr)
 
inEnglish :: Integral a => a -> Maybe String
inEnglish n = if n < 0 || n >= 1000000000000 then Nothing else Just $ spellInteger n
 
spellInteger :: Integral a => a -> String
spellInteger n
 | n <    0  = "negative " ++ spellInteger (-n)
 | n <   20  = small n
 | n <  100  = let (a, b) = n `divMod` 10
               in  tens a ++ nonzero '-' b
 | n < 1000  = let (a, b) = n `divMod` 100
               in  small a ++ " hundred" ++ nonzero ' ' b
 | otherwise = intercalate " " $ map big $ reverse $
               filter ((/= 0) . snd) $ zip [0..] $ unfoldr uff n
 
 where 
       nonzero _ 0 = ""
       nonzero c m = c : spellInteger m
 
       uff 0 = Nothing
       uff m = Just $ uncurry (flip (,)) $ m `divMod` 1000
 
       small = (["zero", "one", "two", "three", "four", "five",
            "six", "seven", "eight", "nine", "ten", "eleven",
            "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
            "seventeen", "eighteen", "nineteen"] !!) . fromEnum
       tens = ([undefined, undefined, "twenty", "thirty", "forty",
           "fifty", "sixty", "seventy", "eighty", "ninety"] !!) .
           fromEnum
 
       big (0, m) = spellInteger m
       big (1, m) = spellInteger m ++ " thousand"
       big (e, m) = spellInteger m ++ ' ' : (l !! e) ++ "illion"
         where l = [undefined, undefined, "m", "b", "tr", "quadr",
                   "quint", "sext", "sept", "oct", "non", "dec"]