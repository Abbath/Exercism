module Frequency where 

import Control.Parallel.Strategies
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Char

g Nothing = Just 1
g (Just n) = Just (n+1)

f s = T.foldl' (\m c -> if isDigit c || isSpace c || isPunctuation c then m else M.alter g (toLower c) m) M.empty s 

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency n text = M.unionsWith (+) $ withStrategy (parBuffer n rdeepseq) $ map f text
