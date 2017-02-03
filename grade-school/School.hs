module School where

import Data.List
import qualified Data.IntMap as IM

type School = [(Int, String)]

empty :: School 
empty = []

add :: Int -> String -> School -> School
add n s [] = [(n,s)]
add n s l = (n,s):l

sorted :: School -> [(Int, [String])]
sorted = IM.toList . IM.map (sort . lines) . IM.fromListWith (\x y -> x ++ "\n" ++ y)

grade :: Int -> School -> [String]
grade n = map snd . filter ((==n) . fst) 