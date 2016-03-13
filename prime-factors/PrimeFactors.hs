module PrimeFactors where

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = let (x, y) = head [ (a, n `div` a) | a <- 2:[3,5..], n `mod` a == 0] 
                 in x : primeFactors y
    