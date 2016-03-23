module Counting where 

import qualified Data.Set as S
import Data.List
import Data.Maybe

data Color = Black | White deriving (Show, Eq, Ord)

data Col = W | B | N | F deriving (Show, Eq)

type Coord = (Int, Int)

territories :: [String] -> [(S.Set Coord, Maybe Color)]
territories arr = nubBy (\(c,_) (d,_) -> c == d) 
    . sort 
    . map fromJust 
    . filter isJust 
    . map (territoryFor arr) 
    $ [(x,y)| x <- [1..length a], y <- [1..length(a!!0)]]  
    where a = transpose arr

territoryFor :: [String] -> Coord -> Maybe (S.Set Coord, Maybe Color)
territoryFor arr (x,y)
    |x < 1 || y < 1 || x > length a || y > length (a !! 0) = Nothing
    where a = transpose arr
territoryFor arr (x,y) = case arr !! (y-1) !! (x-1) of 
                              'B' -> Nothing
                              'W' -> Nothing
                              ' ' -> Just $ search (transpose arr) S.empty (x-1,y-1)
                              _ -> error ""

colToColor :: Col -> Maybe Color
colToColor c = case c of 
                    W -> Just White
                    B -> Just Black
                    N -> Nothing
                    F -> Nothing

fusion :: Col -> Col -> Col
fusion W W = W
fusion B B = B
fusion N W = W
fusion N B = B
fusion W N = W
fusion B N = B
fusion N N = N
fusion _ _ = F
                              
search :: [String] -> S.Set Coord -> Coord ->  (S.Set Coord, Maybe Color)
search a s c = (\(x,y) -> (S.map (\(b,d) -> (b+1,d+1)) x,colToColor y)) $ search' a s N c  
                              
search' :: [String] -> S.Set Coord -> Col -> Coord ->  (S.Set Coord, Col)
search' arr set color (x,y) 
    |test1 (null emptis) blacks whites = sins $ fusion B color 
    |test1 (null emptis) whites blacks = sins $ fusion W color
    |null emptis && not (null whites) && not (null blacks) = sins  F
    |null emptis = sins color
    |color == W && not (null blacks) = fuse F
    |color == B && not (null whites) = fuse F
    |test1 (color == N) blacks whites = fuse B
    |test1 (color == N) whites blacks = fuse W
    |color == F = fuse F
    |otherwise = fuse color
    where 
        space =  map (testCell arr) 
            . filter (`S.notMember` set) 
            . filter (\(i,j) -> i >= 0 && j < length (arr!!0) && j >= 0 && i < length arr)
            $ [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]
        whites = ts $ Just White
        blacks = ts $ Just Black
        emptis = ts Nothing
        ts arg = map fst $ filter ((==arg) . snd) space
        fuse arg = foldl' (\(a,b) (c,d) -> (S.union a c, fusion b d)) (S.empty, N) $ map (search' arr (S.insert (x,y) set) arg) emptis
        test1 f a b = f && not (null a) && null b
        sins arg = (S.insert (x,y) set, arg)
        
testCell :: [String] -> Coord -> (Coord, Maybe Color)
testCell arr (x,y) = ((x,y), case arr !! x !! y of
                                ' ' -> Nothing
                                'B' -> Just Black
                                'W' -> Just White
                                _ -> error "")
