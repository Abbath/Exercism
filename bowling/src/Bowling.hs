{-# LANGUAGE TemplateHaskell, FlexibleContexts, MultiWayIf #-}
module Bowling (score, BowlingError(..)) where

import Control.Monad.State
import Control.Lens

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)
                  
data InnerState = IS { 
    _spare :: Bool , 
    _strike :: Int, 
    _total :: Int, 
    _partial :: Int,
    _frame :: Int, 
    _roll :: Int, 
    _start :: Bool, 
    _bonus :: Int} 
    deriving (Eq, Show)
    
makeLenses ''InnerState

defaultState = IS False 0 0 0 0 0 True 0
    
score :: [Int] -> Either BowlingError Int
score r = evalState (score' r) defaultState

score' :: [Int] -> State InnerState (Either BowlingError Int) 
score' [] = do
    f <- use frame
    r <- use roll
    b <- use bonus
    if f < 10 || b > 0
       then Left <$> return IncompleteGame
       else if f > 10 
            then Left <$> (return $ InvalidRoll (r-1) 0)
            else Right <$> use total
score' s | length s == 12 && all (==10) s = Right <$> return 300
score' (10:xs) = do
    s <- use start
    r <- use roll
    if s 
       then do
         b <- use bonus
         when (b == 0) $ frame += 1
         sp <- use spare
         st <- use strike
         f <- use frame
         roll += 1
         total += if f <= 10 then choose sp st b 10 else 10 
         when (st > 0) $ strike -= 1
         when (b > 0) $ bonus -= 1
         st <- use strike
         strike .= if st /= 0 then 3 else 2
         spare .= False
         partial .= 0     
         start .= True
         when (f == 10 && b == 0) $ bonus .= 2
         score' xs
       else Left <$> (return $ InvalidRoll r 10)  
score' (x:_) | x < 0 || x > 10 = do
    r <- use roll
    Left <$> (return $ InvalidRoll r x)
score' (x:xs) = do
    b <- use bonus
    s <- use start 
    when (s && b == 0) $ frame += 1
    f <- use frame
    roll += 1
    sp <- use spare 
    st <- use strike
    total += if f <= 10 then choose sp st b x else x
    spare .= False
    when (st > 0) $ strike -= 1
    when (b > 0) $ bonus -= 1
    partial += x
    p <- use partial
    start %= not
    r <- use roll
    if not s
       then do
        partial .= 0
        if | p == 10 -> do
               spare .= True
               when (f == 10 && b == 0) $ bonus .= 1
               score' xs 
           | p > 10 -> Left <$> (return $ InvalidRoll (r-1) x)
           | p < 10 -> score' xs
           
       else score' xs
    
choose :: Bool -> Int -> Int -> Int -> Int
choose sp st b x = if b /= 0 then x else x * case (sp,st) of
                      (_,3) -> 3
                      (_,2) -> 2
                      (_,1) -> 2
                      (True,0) -> 2
                      _ -> 1