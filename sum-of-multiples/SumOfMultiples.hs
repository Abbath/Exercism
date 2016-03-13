module SumOfMultiples where

sumOfMultiples :: [Integer] -> Integer -> Integer 
sumOfMultiples s n = sum . filter (\x -> any (\y -> x `mod` y == 0) s) $ [3..(n-1)]

sumOfMultiplesDefault :: Integer -> Integer
sumOfMultiplesDefault n = sumOfMultiples [3, 5] n 