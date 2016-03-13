module BankAccount where

import Data.IORef

type BankAccount = IORef Integer

openAccount :: IO (IORef Integer)
openAccount = newIORef 0

closeAccount :: IORef Integer -> IO ()
closeAccount a = atomicWriteIORef a (-1)

getBalance :: IORef Integer -> IO (Maybe Integer)
getBalance a = do
    n <- readIORef a
    return $ if n < 0 then Nothing else Just n

incrementBalance :: IORef Integer -> Integer -> IO (Maybe Integer)
incrementBalance a m = do
    n <- readIORef a
    if n < 0 
        then return Nothing
        else do
            atomicModifyIORef a (\x -> (x + m, ()))
            return $ Just (n+m)