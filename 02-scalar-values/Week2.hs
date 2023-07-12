
module L2.Week2 where
import Data.Char ( ord, isDigit )

-- ################################################################
-- ## CW 
-- ################################################################

-- Type Definition
data Product = Sun | Planet| Moon
    deriving (Show)

data Customer = Alien1 | Alien2
    deriving (Show)


-- List of products and total in stock stock
sun :: Double
sun = 1

planet :: Double
planet = 8

moon :: Double
moon = 219

totalProduct :: Double
totalProduct = sun + planet + moon

-- Possible solution to stock count
stockCount :: Product -> Int
stockCount Sun = 1
stockCount Planet = 8
stockCount Moon = 219

-- Function 3, Majority: 
-- Checking if customer has ordered more than 50% of product stock
-- Using an AUX function to calculate boolan outcome

moreThanHalf :: Double -> Double -> Double
moreThanHalf sold stock
    | checkPercentageX sold stock >= 0.5   = sold
    | checkPercentageX sold stock <0.5     = stock

-- **** Aux function to calculate 50%
checkPercentageX :: Double -> Double -> Double
checkPercentageX sold stock = sold/stock

overFifty :: String
overFifty = undefined

-- ################################################################
-- ## Lecture
-- ################################################################

small :: Int -> Bool
small n = 0 <= n && n < 10

maxThree :: Int -> Int -> Int -> Int
maxThree x y z
    | x >= y && x >= z = x
    | y >= z = y
    | otherwise = z

maxThreeSimple1 :: Int -> Int -> Int -> Int
maxThreeSimple1 x y z = max x (max y z)

maxThreeSimple2 :: Int -> Int -> Int -> Int
maxThreeSimple2 x y z = x `max` y `max` z


middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
    | between y x z = x
    | between x y z = y
    | otherwise = z

between :: Int -> Int -> Int -> Bool
between x y z = (x <= y && y <= z) || (z <= y && y <= x)

numberOfRoots :: Double -> Double -> Double -> Int
numberOfRoots a b c
    | d < 0 = 0
    | d == 0 = 1
    | otherwise = 2
    where
        d = b*b - 4*a*c


data Colour
    = Red | Green | Blue | Yellow 
    | Cyan | Magenta | Black | White
    deriving (Show)

invert :: Colour -> Colour
invert Red = Cyan
invert Green = Magenta
invert Blue = Yellow
invert Yellow = Blue
invert Cyan = Red
invert Magenta = Green
invert Black = White
invert White = Black


invert1 :: Colour -> Colour
invert1 c = case c of
    Red -> Cyan
    Green -> Magenta
    Blue -> Yellow
    Yellow -> Blue
    Cyan -> Red
    Magenta -> Green
    Black -> White
    White -> Black
   
primary :: Colour -> Bool
primary Red = True
primary Green = True
primary Blue = True
primary c = False


-- ################################################################
-- ## Tutorial
-- ################################################################

-- Q1 Write a function threeDifferent that takes three integer arguments and returns True if its
-- arguments are all different from each other.

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z && x /= z

threeDifferent1 :: Int -> Int -> Int -> Bool
threeDifferent1 x y z = not (x == y || y == z || x == z)

mystery :: Int -> Int -> Int -> Bool
mystery x y z = not (x == y && y == z)

fractional :: Double -> Double
fractional x = x - fromIntegral (floor x)

fractional1 :: Double -> Int
fractional1 x = floor x

clamp1 :: Double -> Double -> Double -> Double
clamp1 lo hi x
    | lo <= x && x <= hi = x
    | x < lo = lo
    | x > hi = hi

clamp2 :: Double -> Double -> Double -> Double
clamp2 lo hi x
    | x < lo = lo
    | x > hi = hi
    | otherwise = x

clamp3 :: Double -> Double -> Double -> Double
clamp3 lo hi x = max lo (min x hi)

charToNum :: Char -> Int
charToNum c
    | isDigit c = ord c - ord '0'
    | otherwise = 0

