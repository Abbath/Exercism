module Spiral (spiral) where

import Data.List
import Control.Applicative

counts = tail . reverse . concat . map (replicate 2) . enumFromTo 1
values = cycle . ((++) <$> map id <*> map negate) . (1 :) . (: [])
grade = map snd . sort . flip zip [1..]
copies = grade . scanl1 (+) . concat . map (uncurry replicate) . (zip <$> counts <*> values)

spiral :: Int -> [[Int]]
spiral = (<*>) take $ (.) <$> (map . take) <*> (iterate . drop) <*> copies

