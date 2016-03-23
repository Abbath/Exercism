{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module SecretHandshake where

import Data.List

handshake :: Bin a => a -> [String]
handshake = f . bin
    where 
        f s
            | length s > 5 = []
            | any (\x -> x /= '1' && x /= '0') s = []
            | otherwise = reverse . go $ s 
        go lel@['1',_,_,_,_] = reverse $ go (tail lel)
        go lel@['1',_,_,_] = "jump" : go (tail lel)
        go lel@['1',_,_] = "close your eyes" : go (tail lel)
        go lel@['1',_] = "double blink" : go (tail lel)
        go "1" = ["wink"]
        go [] = []
        go lel = go $ tail lel
          
class Bin a where
    bin :: a -> String
    
instance Bin String where 
    bin = id
    
instance Bin Int where 
    bin n = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (head $ show $ x `mod` 2, x `div` 2)) $ n  