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
    filter (\c -> isAlphaNum c || isSpace c) .
    map (\c -> if c == '_' || isPunctuation c then ' ' else c) 