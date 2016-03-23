module Matrix where 

import Data.Array

saddlePoints :: Array (Int,Int) Int -> [(Int,Int)]
saddlePoints a = filter (check a) (indices a)

check :: Array (Int,Int) Int -> (Int,Int) -> Bool
check a i = f (<=) fst && f (>=) snd
    where 
        f g h = all (`g` (a ! i)) (map snd . filter (\t -> h (fst t) == h i) $ assocs a)
