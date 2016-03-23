module Triangle where 

row :: Num a => Int -> [a]
row n = triangle !! (n-1) 

zapWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zapWith _ xs [] = xs
zapWith _ [] ys = ys
zapWith f (x:xs) (y:ys) = f x y : zapWith f xs ys

extendWith :: (t -> t -> t) -> [t] -> [t]
extendWith _ [] = []
extendWith f xs@(x:ys) = x : zapWith f xs ys

triangle :: Num a => [[a]]
triangle = iterate (extendWith (+)) [1]
