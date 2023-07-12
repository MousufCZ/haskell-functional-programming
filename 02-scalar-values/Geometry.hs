module L2.Geometry where

-- Q8 (a) Define an enumerated type Direction with 
-- four values, the cardinal compass points.

data Direction = North | South | East | West
    deriving Show


-- (b) and (c) Turning

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight East = South
turnRight West = North

type Point = (Int, Int)
origin :: Point
origin = (0, 0)

