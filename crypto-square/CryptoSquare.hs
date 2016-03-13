module CryptoSquare where 

import Data.Char
import Data.List

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isAlphaNum

squareSize :: String -> Int
squareSize s = head $ dropWhile (\x -> x*x < length s) [1..] 
      
plaintextSegments :: String -> [String]
plaintextSegments s = let ss = normalizePlaintext s
                          size = squareSize ss 
                      in unfoldr (\x -> if x /= [] then Just $ splitAt size x else Nothing ) ss

ciphertext :: String -> String              
ciphertext = concat . cipher

normalizeCiphertext :: String -> String
normalizeCiphertext = intercalate " " . cipher

cipher :: String -> [String]
cipher s = unfoldr (\x -> if any (not . null ) x then (\y -> Just (concatMap fst y, map snd y)) . map (splitAt 1) $ x else Nothing) $ (plaintextSegments s)   
          
