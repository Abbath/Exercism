module PigLatin where 

translate :: String -> String
translate = unwords . map translate' . words

translate' :: String -> String
translate' [] = []
translate' l@(x:_) | x `elem` "aeouiy" = l++"ay"
translate' ('s':'h':xs) = xs ++ "shay"
translate' ('s':'c':'h':xs) = xs ++ "schay"
translate' ('c':'h':xs) = xs ++ "chay"
translate' ('t':'h':'r':xs) = xs ++ "thray"
translate' ('t':'h':xs) = xs ++ "thay"
translate' ('q':'u':xs) = xs ++ "quay"
translate' ('s':'q':'u':xs) = xs ++ "squay"
translate' (x:xs) = xs ++ [x] ++ "ay" 