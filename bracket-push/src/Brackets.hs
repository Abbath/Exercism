module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = go xs []

go [] [] = True
go [] _ = False
go ('(':xs) st = go xs ('(':st)
go ('{':xs) st = go xs ('{':st)
go ('[':xs) st = go xs ('[':st)
go (')':xs) ('(':ys) = go xs ys
go ('}':xs) ('{':ys) = go xs ys
go (']':xs) ('[':ys) = go xs ys
go (x:xs) st = if x `elem` ")}]" then False else go xs st
