module ETL where 

import Data.Map (fromList, toList, Map)
import Data.Char

transform :: Map Int [String] -> Map String Int
transform m = fromList [(map toLower a, x)| (x,z) <- toList m, a <- z]
