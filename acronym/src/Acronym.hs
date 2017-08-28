module Acronym (abbreviate) where

import Data.Char
import Data.List

abbreviate :: String -> String
abbreviate xs = concatMap (map toUpper . select) $ words xs

select :: String -> String
select [] = []
select x | all isUpper (filter isAlpha x) = [head x]
select x | "-" `isInfixOf` x = head x : [head . tail . dropWhile (/='-') $ x]
select x = head x : (filter isUpper . filter isAlpha $ tail x)
