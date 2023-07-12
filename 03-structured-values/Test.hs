module Test where

import Data.List
import Data.Char


data Point = Const Int Int
    deriving (Show)

plusPoint :: Point -> Point -> Point
plusPoint (Const x1 y1) (Const x2 y2) = Const (x1+x2) (y1+y2)



data PriceTag = Item String Double
    deriving (Show)

showPriceTag :: PriceTag -> String
showPriceTag (Item n price) = n ++ " -- " ++ show price

addVAT :: PriceTag -> PriceTag
addVAT (Item n price) = Item n (1.2*price)


data Football = Team String Int
   deriving Show

showTeamPosition :: Football -> String
showTeamPosition (Team teamName tablePosition) = teamName ++ ": Team Table Position -- " ++ show tablePosition

charToNum :: Char -> Maybe Int
charToNum c
  | isDigit c = Just (ord c - ord '0')
  | otherwise = Nothing



-- Lecture 4 Q11 b
foo :: [a] -> [([a], [a])]
foo xs = zip (inits xs) (tails xs)

bar :: [a] -> [[a]]
bar xs = [ys | ts <- tails xs, ys <- inits ts, not (null ys)]

-- Lecture 5 

count :: Eq a => a -> [a] -> Int
count x ys = length [y | y <- ys, y == x]

f :: [Integer] -> [Integer]
f = filter ((< 20) . (^2))


f1 :: [Float] -> [Float]
f1 xs = map (\x -> (x+1)/2) xs

foo1 :: [Int] -> [Int]
foo1 xs = zipWith (-) (tail xs) xs

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = 3*n + 1


collatzSteps :: Int -> Int
collatzSteps n = length (takeWhile (> 1) (iterate collatz n))

collatzMax :: Int -> Int
collatzMax n
  | n <= 1 = 1
  | otherwise = maximum (takeWhile (> 1) (iterate collatz n))



