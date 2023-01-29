-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 15/01/2023
-- Author: Kyle Dick, kd41@hw.ac.uk
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.List
import Distribution.Compat.CharParsing (space)
testSet :: Num a => [([a],(Int,Int))]
testSet = [([4,1,2,3], (2,2)) , ([0,2,3,4], (0,3)), ([1,0,0,0], (0,1))]

-- solve :: [a] -> [([a], (Int,Int))] -> [([a], (Int,Int))]
-- solve code guesses space = case space of
--     [] -> guesses
--     (x :: xs) -> if checkGuess x $ head fst guesses == head snd guesses
--         then if allIdentical (checkConsistency x guesses code)
--             then [(x, checkGuess x code)] : guesses
    
{-
This function evaluates a code based on the hidden combination
If given two lists, it will return a tuple which evaluates the
number of correct elements based on appearance in the first tuple element
and correct element AND position in the second tuple element.
-}
checkGuess :: Eq a => [a] -> [a] -> (Int,Int)
checkGuess guess code = foldl assignPegs (0,0) (zip guess code)
    where 
    assignPegs (black, white) (x, y)
        | x == y = (black + 1, white)
        | x `elem` code = (black, white + 1)
        | otherwise = (black, white)

checkConsistency :: Eq a => [a] -> [a] -> [a] -> Bool
checkConsistency guess code secret = checkGuess guess code == checkGuess code secret

checkConsistencySet :: Eq a => [a] -> [([a], (Int,Int))] -> [a] -> [Bool]
checkConsistencySet code consistentSet secret = let conCodes = map (\(list,_) -> map (\x -> x) list) consistentSet
 in map (\x -> checkGuess code x == checkGuess x secret) conCodes

-- check equality of a list, need to change to universal
allIdentical :: [Bool] -> Bool
allIdentical xs = and xs
 
-- mastermind :: Eq a => [a] -> [a] -> [([a], (Int,Int))] -> [([a], (Int,Int))]
-- mastermind symbols code guesses = 
--     let guess = solveStep symbols guesses
--         check = checkGuess code guess
--     in if check == (0,4)
--         then (guess, (0,4)) : guesses
--         else mastermind symbols code ((guess,check) : guesses)

-- main :: IO()
-- main = do 
--     let x = mastermind ['A','B','C','D','E','F'] ['A','C','D','E'] []
--         in print (length x)