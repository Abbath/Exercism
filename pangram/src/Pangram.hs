module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram text = ['a'..'z'] == (sort . nub . map toLower . filter isAlpha) text
