module L2.Calendar where

-- See Q9 
-- In the Gregorian calendar, a year is a leap year if it is divisible by 4, except that that centuries
-- are leap years only if divisible by 400. Thus 2000 was a leap year, but 2100 won’t be. Write
-- a function

isLeapYear1 :: Int -> Bool
isLeapYear1 y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False

isLeapYear2 :: Int -> Bool
isLeapYear2 y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | otherwise = y `mod` 4 == 0

isLeapYear3 :: Int -> Bool
isLeapYear3 y = y `mod` 400 == 0 || y `mod` 100 /= 0 && y `mod` 4 == 0

-- Q9 (b)

daysInYear :: Int -> Int
daysInYear y
    | isLeapYear1 y = 366
    | otherwise = 365

-- Q9 (c) 
-- (c) Define an enumerated type Month for representing the months of the year.
data Month
    = Jan | Feb | Mar | Apr | May | Jun
    | Jul | Aug | Sep | Oct | Nov | Dec
    deriving Show

-- Q9 (d) 
daysInMonth :: Month -> Int -> Int
daysInMonth Jan y = 31
daysInMonth Feb y
    | isLeapYear1 y = 29
    | otherwise = 28
daysInMonth Mar y = 31
daysInMonth Apr y = 30
daysInMonth May y = 31
daysInMonth Jun y = 30
daysInMonth Jul y = 31
daysInMonth Aug y = 31
daysInMonth Sep y = 30
daysInMonth Oct y = 31
daysInMonth Nov y = 30
daysInMonth Dec y = 31

daysInMonth2 :: Month -> Int -> Int
daysInMonth2 Feb y
    | isLeapYear1 y = 29
    | otherwise = 28
daysInMonth2 Apr y = 30
daysInMonth2 Jun y = 30
daysInMonth2 Sep y = 30
daysInMonth2 Nov y = 30
daysInMonth2 m y = 31