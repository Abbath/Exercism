module Raindrops where

convert :: Int -> String
convert n = case cc n of 
               "" -> show n
               x -> x

cc :: Int -> String
cc = c 3 "Pling" (c 5 "Plang" (c 7 "Plong" (const "")))

c :: Int -> String -> (Int -> String) -> (Int -> String)
c m s f = \n -> if n `mod` m == 0 then s ++ f n else f n  