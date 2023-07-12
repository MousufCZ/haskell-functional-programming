module TestingLetters where

import Data.Char

letters :: [Char] -> [Char]
letters [] = []
letters (c:cs)
  | isAlpha c = c : letters cs
  | otherwise = letters cs


dropLetters :: [Char] -> [Char]
dropLetters [] = []
dropLetters (c:cs)
  | isAlpha c = dropLetters cs
  | otherwise = c:cs
