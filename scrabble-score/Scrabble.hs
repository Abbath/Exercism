module Scrabble where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Char
import Data.List

table :: Map String Int
table = M.fromList [(a, x)| (x,z) <- table', a <- z]

table' :: [(Int, [String])]
table' = [(a, map (:[]) b) |(a,b) <- [
    (1, "AEIOULNRST"),
    (2, "DG"),
    (3, "BCMP"),
    (4, "FHVWY"),
    (5, "K"),
    (8, "JX"),
    (10, "QZ")]]

scoreLetter :: Char -> Int
scoreLetter c =  table M.! [toUpper c]

scoreWord :: String -> Int
scoreWord = foldl' (+) 0 . map scoreLetter