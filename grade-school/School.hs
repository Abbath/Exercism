module School where

import Data.List
import Data.Function
import qualified Data.Set as S

type School = [(Int, String)]

empty :: School 
empty = []

add :: Int -> String -> School -> School
add n s [] = [(n,s)]
add n s l = (n,s):l

sorted :: School -> [(Int, [String])]
sorted l = reverse $ map (\(x,y) -> (x,S.toList y)) $ foldl gr [] $ sortBy (compare `on` fst) l

gr :: [(Int, S.Set String)] -> (Int, String) -> [(Int, S.Set String)]
gr [] (c,d) = [(c,S.singleton d)]
gr x@((a,b):xs) (c,d) = if a == c then (a,S.insert d b):xs else (c,S.singleton d):x

grade :: Int -> School -> [String]
grade n = map snd . filter (\(nn,_) -> n == nn) 