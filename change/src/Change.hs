module Change (findFewestCoins) where

import Data.List

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = go target (sortBy (flip compare) coins) [] Nothing
    where add t cs@(c:_) cn | t >= c = go (t-c) cs (c:cn)
          add _ _ _ = id
          dr t (_:xs) = go t xs
          dr _ _ = \_ x -> x
          go t c cn r
            |t < 0 || maybe False ((<= length cn) . length) r = r
            |t == 0 = Just cn
            |otherwise = dr t c cn (add t c cn r)
