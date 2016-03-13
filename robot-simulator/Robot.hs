module Robot where

data Bearing = East | South | West | North deriving (Show, Eq) 

type Robot = (Bearing, (Int, Int))

mkRobot :: Bearing -> (Int, Int) -> Robot
mkRobot = (,) 

coordinates :: Robot -> (Int, Int)
coordinates = snd

bearing :: Robot -> Bearing
bearing = fst

turnRight :: Bearing -> Bearing
turnRight East = South
turnRight South = West
turnRight West = North 
turnRight North = South

turnLeft :: Bearing -> Bearing
turnLeft East = North
turnLeft South = East
turnLeft West = South 
turnLeft North = West

charToAction :: Robot -> Char -> Robot
charToAction (b,c) 'R' = (turnRight b, c)
charToAction (b,c) 'L' = (turnLeft b, c)
charToAction (b,(x,y)) 'A' = case b of 
    East -> (b, (x+1,y))
    South -> (b, (x, y-1))
    West -> (b, (x-1,y))
    North -> (b, (x, y+1))
charToAction _ _ = error "Wrong command!"

simulate :: Robot -> String -> Robot
simulate = foldl charToAction
              