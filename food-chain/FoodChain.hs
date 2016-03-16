module FoodChain where

import Data.List

line :: String -> String -> String
line a b = "She swallowed the " ++ a ++ " to catch the " ++ b ++ ".\n"

lins :: [String]
lins =
    [
    line "cow" "goat",
    line "goat" "dog",
    line "dog" "cat",
    line "cat" "bird",
    line "bird" "spider that wriggled and jiggled and tickled inside her",
    line "spider" "fly",
    "I don't know why she swallowed the fly. Perhaps she'll die.\n"
    ]

starts :: [String]
starts = zipWith (++) (repeat "I know an old lady who swallowed a ") [
    "fly.\n",
    "spider.\nIt wriggled and jiggled and tickled inside her.\n",
    "bird.\nHow absurd to swallow a bird!\n",
    "cat.\nImagine that, to swallow a cat!\n",
    "dog.\nWhat a hog, to swallow a dog!\n",
    "goat.\nJust opened her throat and swallowed a goat!\n",
    "cow.\nI don't know how she swallowed a cow!\n",
    "horse.\nShe's dead, of course!\n"
    ]
    
song :: String
song = (++"\n") . concat . intercalate ["\n"] $ zipWith (:) starts $ (++[[]]) . tail . reverse $ tails $ lins


