module Main where

import System.Random

getInt :: IO Int
getInt = readLn

maxValue :: Int
maxValue = 100

main :: IO ()
main = do
    n <- randomIO    -- from System.Random
    guessingGame (n `mod` maxValue + 1)

guessingGame :: Int -> IO ()
guessingGame target = do
    putStr $ "Guess a number between 1 and " ++ show maxValue ++ ": "
    guesses target 1

guesses :: Int -> Int -> IO ()
guesses target nguesses = do
    guess <- getInt
    if guess == target then
        putStrLn $ "Correct in " ++ show nguesses ++ " guesses"
    else do
        putStr $ if guess > target
            then "Too high! " else "Too low! "
        putStr "Guess again: "
        guesses target (nguesses+1)
