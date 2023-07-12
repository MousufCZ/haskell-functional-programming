{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module L3.Geometry where

-- L2 Q8 (a) Define an enumerated type Direction with 
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

-- L3

data Point = Point Int Int
    deriving (Show)

origin :: Point
origin = Point 0 0

plusPoint :: Point -> Point -> Point
plusPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

minusPoint :: Point -> Point -> Point
minusPoint (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

timesPoint :: Int -> Point -> Point
timesPoint n (Point x y) = Point (n*x) (n*y)

-- Scale Point that computes the sum of the absolute values of 
-- the two components. This is called the Manhattan metric
normPoint :: Point -> Int
normPoint (Point x y) = abs x + abs y

-- that computes the distance between two points using the 
-- Manhattan metric. 
distance :: Point -> Point -> Int
distance p1 p2 = normPoint (minusPoint p1 p2)


-- that maps each direction to the point one unit from the 
-- origin in that direction
oneStep :: Direction -> Point
oneStep North = Point 0 1
oneStep East = Point 1 0
oneStep South = Point 0 (-1)
oneStep West = Point (-1) 0



data Turtle = Turtle Point Direction PenState
    deriving Show

data PenState = PenUp | PenDown
    deriving Show

-- Setting initial state
startTurtle :: Turtle
startTurtle = Turtle origin North PenUp

-- Currently unused
location :: Turtle -> Point
location (Turtle pos dir pen) = pos

-- Q4 - (d)
-- Defining a type 
data Command
    = TurnLeft | TurnRight | Move Int | RaisePen | LowerPen
    deriving Show

-- Q5 - (e)
action :: Turtle -> Command -> Turtle
action (Turtle pos dir pen) TurnLeft = Turtle pos (turnLeft dir) pen
action (Turtle pos dir pen) TurnRight = Turtle pos (turnRight dir) pen
action (Turtle pos dir pen) (Move n) = Turtle (plusPoint pos (timesPoint n (oneStep dir))) dir pen
action (Turtle pos dir _) RaisePen = Turtle pos dir PenUp
action (Turtle pos dir _) LowerPen = Turtle pos dir PenDown

readGrid :: String -> [(Point, Char)]
readGrid s =
    [(Point x y, c) | (y, cs) <- zip [0,-1..] (lines s),
    (x, c) <- zip [0..] cs]