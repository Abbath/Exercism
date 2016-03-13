{-# LANGUAGE FlexibleInstances #-}
module Matrix where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List.Split
import Data.List
import Data.Maybe

data Matrix a = Matrix Int Int (Vector a) deriving (Show, Eq)

shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = if (r,c) == (1,0) then (0,0) else (r, c)

row :: Int -> Matrix a -> Vector a
row n (Matrix r c v) = V.slice (n*c) c v

rows :: Matrix a -> Int
rows (Matrix r _ _) = r

cols :: Matrix a -> Int
cols (Matrix _ c _) = c

column :: Int -> Matrix a -> Vector a
column n (Matrix r c v) = V.generate r (\x -> v V.! (x*c+n)) 

transpose :: Matrix a -> Matrix a
transpose mm = Matrix (cols mm) (rows mm) $ V.foldr (V.++) V.empty $ V.generate (cols mm) (\x -> column x mm)

flatten :: Matrix a -> Vector a
flatten (Matrix _ _ v) = v
 
fromList :: [[a]] -> Matrix a
fromList l = Matrix (length l) (length $ l !! 0) (V.fromList . concat $ l)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix _ _ v)  = Matrix r c v 

fromString :: Read a => String -> Matrix a
fromString = fromList . map (unfoldr (listToMaybe . reads)) . splitOn "\n"