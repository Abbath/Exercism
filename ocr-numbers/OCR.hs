module OCR where

import Data.List
import Data.Maybe

zero, one, two, three, four, five, six, seven, eight, nine :: [String]
zero = [" _ ", "| |", "|_|" , "   "]
one = ["   ", "  |", "  |", "   "]
two = [" _ ", " _|", "|_ ", "   "]
three = [" _ ", " _|", " _|", "   "]
four = ["   ", "|_|", "  |", "   "]
five = [" _ ", "|_ ", " _|", "   "]
six = [" _ ", "|_ ", "|_|", "   "]
seven = [" _ ", "  |", "  |", "   "]
eight = [" _ ", "|_|", "|_|", "   "]
nine = [" _ ", "|_|", " _|", "   "]

digits :: [([String], Char)]
digits = zip [zero, one, two, three, four, five, six, seven, eight, nine] ['0'..'9']

convert :: String -> String
convert s | length (lines s) == 4 = unfoldr (\x -> if null (x !! 0) then Nothing else Just (fromMaybe '?' . lookup (map (take 3) x) $ digits, map (drop 3) x)) (lines s) 
convert s = intercalate "," $ unfoldr (\x -> if null x then Nothing else Just (convert (unlines $ take 4 x), drop 4 x)) (lines s) 