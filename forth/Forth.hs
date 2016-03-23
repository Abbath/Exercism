{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
  , empty
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import qualified Data.Map as M

data ForthState = ForthState [Int] (M.Map String [String])-- TODO: define this data type

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

empty :: ForthState
empty = ForthState [] M.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText t (ForthState s m) = eval (parse t) s m

parse :: Text -> [String]
parse t = words . map toLower . shyza . map (\c-> if isControl c then ' ' else c) $ T.unpack t 

shyza :: String -> String
shyza [] = []
shyza (x:'-':z:xs) | isDigit x && isDigit z = x:' ':z: shyza xs
shyza (x:xs) = x : shyza xs

eval :: [String] -> [Int] -> M.Map String [String] -> Either ForthError ForthState
eval [] s m = Right (ForthState s m) 
eval (op:xs) s m | op `M.member` m = eval ((m M.! op) ++ xs) s m 
eval ("over":_) s _ | length s < 2 = Left StackUnderflow
eval ("over":xs) (x:y:ys) m = eval xs (y:x:y:ys) m
eval ("dup":_) [] _ = Left StackUnderflow
eval ("dup":xs) (y:ys) m = eval xs (y:y:ys) m
eval ("swap":_) s _ | length s < 2 = Left StackUnderflow 
eval ("swap":xs) (y:yy:ys) m = eval xs (yy:y:ys) m
eval ("drop":xs) (_:ys) m = eval xs ys m
eval ("drop":_) [] _ = Left StackUnderflow
eval ("+":xs) (a:b:ys) m = eval xs (a+b:ys) m
eval ("-":xs) (a:b:ys) m = eval xs (b-a:ys) m
eval ("*":xs) (a:b:ys) m = eval xs (a*b:ys) m
eval ("/":_) (0:_:_) _ = Left DivisionByZero
eval ("/":xs) (a:b:ys) m = eval xs (b`div`a:ys) m
eval (":":name:xs) s m = if valid name then addName (name:xs) s m else Left InvalidWord
eval (n:xs) s m | isNum n = eval xs ((read n :: Int) :s) m
eval (op:_) _ m | op `M.notMember` m = Left $ UnknownWord (T.pack op)
eval _ _ _ = error "Bad thing is happening wright now!"

addName :: [String] -> [Int] -> M.Map String [String] -> Either ForthError ForthState
addName (n:xs) s m = let (body, _:rest) = break (==";") xs
                     in eval rest s  (M.insert n body m)   
addName _ _ _ = error "Bad name!"

valid :: String -> Bool
valid [] = False
valid (h:_) = (not . isDigit) h -- && all (\c -> isAlpha c || c == '-') t 
                     
isNum :: String -> Bool
isNum s = let t = (reads s :: [(Int,String)])
             in t /= [] && [] == snd (head t) 

formatStack :: ForthState -> Text
formatStack (ForthState s _) = T.pack . unwords . map show $ reverse s

