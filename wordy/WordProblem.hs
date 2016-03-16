module WordProblem where 

import Data.String.Utils
import Text.Read

answer :: String -> Maybe Int
answer = go . format
  
go :: [String] -> Maybe Int
go [] = Nothing    
go [n] = readMaybe n :: Maybe Int
go (a:x:b:xs) = case x of  
    "+" -> f (+)  
    "-" -> f (-)
    "*" -> f (*)
    "/" -> f (div)
    _ -> Nothing
    where 
        f g = do 
            t <- g <$> rmb a <*> rmb b
            go $ show t : xs
go _ = Nothing
   
rmb :: String -> Maybe Int
rmb n = readMaybe n :: Maybe Int
    
format :: String -> [String]
format = words
    . replace "multiplied by" "*"
    . replace "divided by" "/" 
    . replace "minus" "-" 
    . replace "plus" "+" 
    . replace "?" "" 
    . replace "What is " ""