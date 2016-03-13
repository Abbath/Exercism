module DNA where

toRNA :: String -> String
toRNA = map f
    where f c = case c of 
            'G' -> 'C'
            'C' -> 'G'
            'T' -> 'A'
            'A' -> 'U'
            x -> x