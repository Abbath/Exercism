module Allergies where

import Data.Bits
import Data.Maybe

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate | Pollen | Cats deriving (Show, Eq, Enum)

list :: [(Allergen, Int)]
list = zip [Eggs ..Cats] [0::Int ..]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a n = testBit n (fromMaybe 0 . lookup a $ list) 

allergies :: Int -> [Allergen]
allergies n = filter (`isAllergicTo` n) [Eggs ..Cats]