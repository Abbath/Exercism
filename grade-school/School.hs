module School where

import Data.List
import Data.Function

type School = [(Int, String)]

empty :: School 
empty = []

add :: Int -> String -> School -> School
add n s [] = [(n,s)]
add n s l = ((n,s):l)

sorted :: School -> [(Int, [String])]
sorted l = reverse $ foldl gr [] $ sortBy (compare `on` fst) l

gr :: [(Int, [String])] -> (Int, String) -> [(Int, [String])]
gr [] (c,d) = [(c,[d])]
gr x@((a,b):xs) (c,d) = if a == c then ((a,sort (d:b)):xs) else ((c,[d]):x)

grade :: Int -> School -> [String]
grade n l = map snd . filter (\(nn,_) -> n == nn) $ l 