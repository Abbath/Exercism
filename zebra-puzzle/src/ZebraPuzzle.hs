module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad
import Data.List

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Bounded, Enum, Eq, Show)
  
data Color = Red | Green | Ivory | Blue | Yellow
  deriving (Bounded, Enum, Eq, Show)
  
data Animal = Dog | Fox | Horse | Zebra | Snails
  deriving (Bounded, Enum, Eq, Show)
  
data Drink = Coffee | Tea | Milk | Juice | Water
  deriving (Bounded, Enum, Eq, Show)
  
data Smoke = OldGold | Kools | LuckyStrike | Chesterfields | Parliaments 
  deriving (Bounded, Enum, Eq, Show)
  
data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

values :: (Bounded a, Enum a) => [[a]]
values = permutations [minBound..maxBound]
                         
answers = do
    color <- values
    rightOf color Green color Ivory
    
    resident <- values 
    same resident Englishman color Red
    first resident Norwegian
    nextTo resident Norwegian color Blue
    
    drink <- values
    same drink Coffee color Green
    same resident Ukrainian drink Tea
    middle drink Milk
    
    animal <- values
    same resident Spaniard animal Dog
    
    smoke <- values
    same smoke OldGold animal Snails
    same smoke Kools color Yellow
    nextTo smoke Chesterfields animal Fox
    nextTo smoke Kools animal Horse
    same smoke LuckyStrike drink Juice
    same resident Japanese smoke Parliaments
    
    return $ zip5 resident color animal drink smoke
    where
        same xs x ys y = guard $ (x,y) `elem` zip xs ys
        rightOf xs x ys y = same (tail xs) x ys y
        nextTo xs x ys y = rightOf xs x ys y `mplus` rightOf ys y xs x
        middle xs x = guard $ xs !! 2 == x
        first xs x = guard $ head xs == x
                         
solve :: Solution
solve = let ans = head answers in Solution (get $ filter (\(_,_,_,x,_) -> x == Water) ans) (get $ filter (\(_,_,x,_,_) -> x == Zebra) ans)
    where get [(a,_,_,_,_)] = a 
