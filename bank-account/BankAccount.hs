module BankAccount where

import Control.Concurrent.MVar

type BankAccount = MVar (Maybe Integer)

openAccount :: IO (MVar (Maybe Integer))
openAccount = newMVar (Just 0)

closeAccount :: MVar (Maybe Integer) -> IO ()
closeAccount a = modifyMVar_ a (\_ -> return Nothing)

getBalance :: MVar (Maybe Integer) -> IO (Maybe Integer)
getBalance = readMVar


incrementBalance :: MVar (Maybe Integer) -> Integer -> IO (Maybe Integer)
incrementBalance a m = modifyMVar a (\x -> return $ (fmap (+m) x, fmap (+m) x))