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

--type Point = (Int, Int)

--type Pair a = (a, a)
--type Point = Pair Int

-- Data definition
--data Point = MkPoint Int Int
--    deriving (Show)

--origin :: Point
--origin = (0, 0) 

--plusPoint :: Point -> Point -> Point
--plusPoint (x1, y1) (x2, y2) = (x1+x2, y1+y2)

--plusPoint1 :: Point -> Point -> Point
--plusPoint1 p1 p2 = (fst p1 + fst p2, snd p1 + snd p2)


data Point = Point Int Int
    deriving (Show)

origin :: Point
origin = Point 0 0

plusPoint :: Point -> Point -> Point
plusPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

