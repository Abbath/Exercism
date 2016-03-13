module Squares where 

sumOfSquares :: (Integral a) => a -> a
sumOfSquares n = n*(n+1)*(2*n+1) `div` 6

squareOfSums :: (Integral c) => c -> c
squareOfSums n = (^2) $ (n+1) * (n `div` 2) + (if even n then 0 else n `div` 2 + 1) 

difference :: (Integral a) => a -> a
difference n = squareOfSums n - sumOfSquares n