module Lecture7 where

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (n:ns) = double n : doubleAll ns
  where double x = 2*x



odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (x1:x2:x3:xs) = x1:odds xs

evens :: [a] -> [a]
evens [] = []
evens [x] = []
evens (x1:x2:x3:xs) = x2:evens xs

third :: [a] -> [a]
third [] = []
third [x] = []
third (x1:x2:x3:xs) = x3:evens xs


data NTree a = Empty | Node a (NTree a) (NTree a)
  deriving Show
