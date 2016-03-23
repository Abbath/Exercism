module Cipher where 

import Data.Char
import System.Random

getOrd :: Char -> Int
getOrd c = ord c - ord 'a'

encodeDecode :: (Int -> Int -> Int) -> String -> String -> String
encodeDecode f k t = zipWith  (\x y -> chr $ ord 'a' + ((ord y `f` x - ord 'a' ) `mod` 26)) (cycle $ map getOrd k) t

caesarEncode :: String -> String -> String
caesarEncode = encodeDecode (+)

caesarDecode :: String -> String -> String
caesarDecode = encodeDecode (-)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    gen <- newStdGen
    let k = randomRs ('a','z') gen
    return $ (k,encodeDecode (+) k text)