module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

type Coordinates = (Integer, Integer)
type Robot = (Bearing, Coordinates)

bearing :: Robot -> Bearing
bearing (b, _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (_, c) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = (direction, coordinates)

turnFn :: Bearing -> Char -> Bearing
turnFn North 'R' = East
turnFn North 'L' = West
turnFn East  'R' = South
turnFn East  'L' = North
turnFn South 'R' = West
turnFn South 'L' = East
turnFn West  'R' = North
turnFn West  'L' = South
turnFn d      _  = d

advanceFn :: Robot -> Char -> Coordinates
advanceFn (North, (x, y)) 'A' = (x  , y+1)
advanceFn (South, (x, y)) 'A' = (x  , y-1)
advanceFn (East , (x, y)) 'A' = (x+1, y  )
advanceFn (West , (x, y)) 'A' = (x-1, y  )
advanceFn r                _  = coordinates r

moveFn :: Robot -> Char -> Robot
moveFn r c = mkRobot (turnFn (bearing r) c) (advanceFn r c) 

move :: Robot -> String -> Robot
move r i = foldl moveFn r i 
