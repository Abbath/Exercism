module Alphametics (solve) where

import Control.Monad
import Control.Applicative
import Control.Arrow (first)
import Data.Maybe (fromJust)
import Data.List (nub)
import Data.Char
import qualified Data.Map.Strict as M

newtype StateL s a = StateL (s -> [(a, s)])

solve :: String -> Maybe [(Char,Int)]
solve puzzle = case evalStateL (solveState puzzle) [0..9] of
                    [] -> Nothing
                    x -> Just . head $ x

runStateL :: StateL s a -> s -> [(a, s)]
runStateL (StateL g) = g

evalStateL :: StateL s a -> s -> [a]
evalStateL (StateL g) s = fmap fst (g s)

instance Functor (StateL s) where
    fmap f (StateL g) = StateL $ fmap (first f) . g

instance Applicative (StateL s) where
    pure x = StateL $ \s -> [(x, s)]
    fs <*> xs = StateL $ \s -> [(f a, s'') | (f, s' ) <- runStateL fs s
                                           , (a, s'') <- runStateL xs s]

instance Alternative (StateL s) where
    empty = StateL $ const []
    as <|> bs = StateL $ \s -> runStateL as s ++ runStateL bs s

instance Monad (StateL s) where
    return = pure
    --(StateL g) >>= k = StateL $ concat . fmap (\(a, s) -> runStateL (k a) s) . g
    (StateL g) >>= k = StateL $ \s -> [(b, s'') | (a, s' ) <- g s
                                                , (b, s'') <- runStateL (k a) s']

instance MonadPlus (StateL s) where
    mzero = empty
    mplus = (<|>)

select ::[a] ->[(a, [a])]
select [] = []
select(x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <-select xs]

filterLetters :: String -> String
filterLetters = nub . filter isAlpha

parse :: String -> [String]
parse s = words $ map (\c -> if not $ isAlpha c then ' ' else c) $ filter (/=' ') s

zipp :: [String] -> [Int] -> [(Char,Int)]
zipp a b = nub $ concat $ zipWith (\x y -> zip x (map (\x -> read [x] :: Int) $ show y)) a b
    
solveState :: String -> StateL [Int] [(Char,Int)]
solveState s = StateL select >>= go (filterLetters s) M.empty
  where
    go [c] subst i = prune (M.insert c i subst)
    go (c:cs) subst i = StateL select >>= go cs (M.insert c i subst)
    prune subst = do
        let parsed = parse s 
        let heads = map head parsed
        guard (all id $ map (\x -> get x /= 0) heads)
        let r = map toNumber parsed
        guard $ sum (init r) == last r 
        return $ zipp parsed r
      where
        get c = fromJust (M.lookup c subst)
        toNumber str = asNumber (map get str)
        asNumber = foldl (\t o -> t*10 + o) 0