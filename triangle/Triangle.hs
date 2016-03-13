module Triangle where 

import Data.List

data TriangleType = Equilateral | Isosceles | Scalene | Illogical deriving (Show, Eq)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c = test (sort [a, b, c])
    where 
        test [x, y, z] 
            | x + y <= z = Illogical
            | x == y && x == z = Equilateral
            | x == y || y == z = Isosceles
            | otherwise = Scalene
        test _ = error "Wrong data!"
        
    