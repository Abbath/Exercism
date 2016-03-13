module Robot where 

import System.Random
import Data.IORef

type Robot = IORef String

robotName :: Robot -> IO String
robotName name = readIORef name

mkRobot :: IO Robot
mkRobot = do
    name <- generateName
    newIORef name 
    
resetName :: Robot -> IO Robot
resetName r = do
    newName <- generateName
    writeIORef r newName 
    return r
    
generateName :: IO String
generateName = do
    mapM randomRIO [('A', 'Z'), ('A', 'Z'), ('0', '9'), ('0', '9'), ('0', '9')]
    