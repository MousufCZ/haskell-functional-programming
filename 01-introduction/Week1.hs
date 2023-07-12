module Week1 where
import System.Win32 (xBUTTON1, COORD (yPos))
import Distribution.SPDX (LicenseId(BlueOak_1_0_0))

size :: Integer
size = 12 + 13

square :: Integer -> Integer
square n = n*n

triple :: Integer -> Integer
triple n = 3*n

squareOfTriple :: Integer -> Integer
squareOfTriple n = square (triple n)

norm :: Double -> Double -> Double
norm x y = sqrt (x*x + y*y)

small :: Int -> Bool
small n = 0 <= n && n < 10
    
maxThreeBeforeSim :: Int -> Int -> Int -> Int
maxThreeBeforeSim x y z
    | x >= y && x >= z  = x
    | y >= x && y >= z  = y
    | otherwise         = z

max2 :: Int -> Int -> Int
max2 x y
    | x >= y    = x
    | otherwise = y

maxThree :: Int -> Int -> Int -> Int
maxThree x y z
    | x >= y && x >= z  = x
    | y >= z            = y
    | otherwise         = z

maxThreeVersion :: Int -> Int -> Int -> Int
maxThreeVersion x y z = max2 x (max2 y z)

maxThreeEasier :: Int -> Int -> Int -> Int
maxThreeEasier x y z = x `max` y `max` z

-- ### Slide 23 
-- ### Developing a function
-- ###======================

middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
    | between y x z = x
    | between x y z = y
    | otherwise     = z

between :: Int -> Int -> Int -> Bool
between x y z =
    (x <= y && y <= z) || (z <= y && y <= x)

f :: Int -> Int
f x = let y = x*x + 1 in y*y

fVersion :: Int -> Int
fVersion x = y*y
    where
        y = x*x + 1


-- ### Lecture 2 - Slide 28 
-- ### Where and guards
-- ###======================
-- number of real roots of a quadratic equation
-- a*x^2 + b*x + c = 0

numberOfRoots :: Double -> Double -> Double -> Int
numberOfRoots a b c
    | d < 0     = 0
    | d == 0    = 1
    | otherwise = 2
    where 
        d = b*b - 4*a*c

-- ### Lecture 2 - Slide 29 
-- ### Enumerated types
-- ###======================
data Colour
    = Red | Green | Blue | Yellow
    | Cyan | Magenta | Black | White
    deriving (Show)

invert :: Colour -> Colour
invert c = case c of
    Red -> Cyan
    Green -> Magenta
    Blue -> Yellow
    Yellow -> Blue
    Cyan -> Red
    Magenta -> Green
    Black -> White
    White -> Black


-- ### Lecture 2 - Slide 33 
-- ### Primary colours
-- ###======================

primary :: Colour -> Bool
primary Red = True
primary Green = True
primary Blue = True
primary c = False

