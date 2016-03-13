module House where 

import Data.List

thisLine :: String -> String
thisLine = ("This is the " ++)

thatLine :: String -> String -> String
thatLine action subject = "that " ++ action ++ " the " ++ subject

houseLine :: String
houseLine = "that lay in the house that Jack built.\n\n"

things :: [(String, String)]
things =
    [
    ("", "horse and the hound and the horn"),
    ("belonged to", "farmer sowing his corn"),
    ("kept", "rooster that crowed in the morn"),
    ("woke", "priest all shaven and shorn"),
    ("married", "man all tattered and torn"),
    ("kissed", "maiden all forlorn"),
    ("milked", "cow with the crumpled horn"),
    ("tossed", "dog"),
    ("worried", "cat"),
    ("killed", "rat"),
    ("ate", "malt")
    ]

rhyme :: String    
rhyme = 
    ("This is the house that Jack built.\n\n"++) 
    . intercalate ""
    . map (intercalate "\n")
    . tail
    . reverse
    . map ((++ [houseLine] ).headLine) $ (tails things) 

headLine :: [(String, String)] -> [String]
headLine [] = []
headLine (x:xs) = thisLine (snd x) : map (uncurry thatLine) xs

-- rhyme
