module Primes where 

import Data.Vector.Unboxed hiding (forM_, foldl, reverse, filter)
import Data.Vector.Unboxed.Mutable
import Control.Monad.ST (runST)
import Control.Monad (forM_, when)
import Prelude hiding (read)

sieve :: Int -> Vector Bool
sieve n = runST $ do
    vec <- new (n + 1) -- Create the mutable vector
    set vec True       -- Set all the elements to True
    write vec 1 False  -- One isn't a prime
    forM_ [2..n] $ \ i -> do -- Loop for i from 2 to n
        val <- read vec i -- read the value at i
        when val $ -- if the value is true, set all its multiples to false
          forM_ [2*i, 3*i .. n] $ \j -> write vec j False
    freeze vec -- return the immutable vector
    
nth n = sieve n ! n