module WordCount where

import qualified Data.Map as M
import Data.List
import Data.Char

wordCount :: String -> M.Map String Int
wordCount = M.fromList .
    map (\l -> (head l, length l)) .
    group .
    sort .
    words .
    map toLower .
    map (\c -> if c == '_' || isPunctuation c then ' ' else c ) .
    filter (\c -> c == '_' || isAlphaNum c || isSpace c || isPunctuation c)