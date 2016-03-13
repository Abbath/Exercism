module Garden where

import Data.List
import Data.Maybe

data Plant = Clover | Grass | Radishes | Violets deriving (Show, Eq)

charToPlant :: Char -> Plant
charToPlant s = case s of 
    'V' -> Violets
    'C' -> Clover
    'G' -> Grass
    'R' -> Radishes
    _ -> error ""

garden :: [String] -> String -> ([String], String)
garden k p = (sort k, p)

defaultGarden :: String -> ([String], String)
defaultGarden p = (take ((length p - 1) `div` 4) kids, p) 

lookupPlants :: String -> ([String], String) -> [Plant]
lookupPlants k g = let n = fromMaybe (-1) $ elemIndex k (fst g) 
                   in map charToPlant . map ((snd g)!!) $ [n*2,n*2+1, n*2+(1+length (fst g) * 2),1+n*2+(1+length (fst g) * 2)]  

kids :: [String]
kids = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

