module LeapYear where

isLeapYear n = n `mod` 4 == 0 && not (n `mod` 100 == 0) || n `mod` 400 == 0