module Clock where 

data Clock = Clock Integer Integer deriving (Show, Eq)

instance Num Clock where
    (+) (Clock h1 m1) (Clock h2 m2) = let x = (h1+h2)*60 + m1 + m2 
                                      in Clock (x `div` 60 `mod` 24) (x `mod` 60)
    (-) (Clock h1 m1) (Clock h2 m2) = let x = (h1*60+m1) - (h2*60+m2) 
                                      in if x < 0 
                                         then Clock ((24*60+x) `div` 60 `mod` 24) ((24*60+x) `mod` 60)
                                         else Clock (x `div` 60 `mod` 24) (x `mod` 60)
    fromInteger n = Clock (n `div` 60 `mod` 24) (n `mod` 60)
    
fromHourMin :: Integer -> Integer -> Clock
fromHourMin h m = fromInteger (h*60+m)

toString :: Clock -> String
toString (Clock h m) = 
    (if h < 10 then "0" else "")
    ++ show h 
    ++ ":" 
    ++ (if m < 10 then "0" else "") 
    ++ show m  
