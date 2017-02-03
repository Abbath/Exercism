module Base (rebase) where

import Data.List
import Data.Tuple

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits | inputBase <= 1 || outputBase <= 1 = Nothing
rebase _ _ [] = Just []
rebase _ _ x | all (==0) x = Just []
rebase _ _ c | any (<0) c = Nothing 
rebase i _ c | any (>=i) c = Nothing 
rebase a b (0:xs) = rebase a b xs
rebase a b c = let n = length c - 1
                   s = sum $ zipWith (\x y -> x * a^y) c [n,n-1..0] 
               in Just . reverse $ unfoldr (\x -> if x == 0 then Nothing else Just . swap $ divMod x b) s
    
