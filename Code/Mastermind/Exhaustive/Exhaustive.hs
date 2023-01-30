-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 15/01/2023
-- Author: Kyle Dick, kd41@hw.ac.uk
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.List
import Distribution.Compat.CharParsing (space)
import Control.Monad (replicateM)

-- functions for use in testing using the interactive environment
testSet :: Num a => [([a],(Int,Int))]
testSet = [([4,1,2,3], (2,2)) , ([0,2,3,4], (0,3)), ([1,0,0,0], (0,1))]
testSpace :: Num a => [[a]]
testSpace = [[1,2,3,3],[3,0,3,2],[0,3,2,3],[3,0,1,3],[4,4,4,4],[1,0,4,0]]

-- this function returns the next guess at the current step
-- inputs the secret code, the previous guesses and all possible answers at the current step
solveStep :: Eq a => [a] -> [([a], (Int,Int))] -> [[a]] -> [a]
solveStep secret guesses space = head (pruneInconsistent (fst $ head guesses) secret space)

-- returns the evaluation of a guess with respect to the secret code
-- (Black, White) where Black is a count of exact matches between the two list elements
--  White is a count of elements which both elements share but are not in the same positions
checkGuess :: Eq a => [a] -> [a] -> (Int,Int)
checkGuess guess code = (checkBlack guess code, length $ checkWhite guess code)

checkWhite :: Eq a => [a] -> [a] -> [Int]
checkWhite guess code = findIndices (\(x,y) -> x `elem` code && x /= y) (zip guess code)

checkBlack :: Eq a => [a] -> [a] -> Int
checkBlack guess code = length [x | (x,y) <- zip guess code, x == y]

-- Consistency Functions --
-- Consistency means that for a code c and previous code prevC
--  checkGuess c prevC == checkGuess prevC secretCode

-- function to evaluate if a guess is consistent with another with respect to the secret code
checkConsistency :: Eq a => [a] -> [a] -> [a] -> Bool
checkConsistency guess code secret = checkGuess guess code == checkGuess code secret

-- function which checks if a new guess is consistent with all previous guesses with respect to the secret code
checkConsistencySet :: Eq a => [a] -> [([a], (Int,Int))] -> [a] -> [Bool]
checkConsistencySet code consistentSet secret = let conCodes = map (\(list,_) -> map (\x -> x) list) consistentSet
 in map (\x -> checkGuess code x == checkGuess x secret) conCodes

-- removes all inconsistent 
pruneInconsistent :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
pruneInconsistent code secret space = filter (\x -> checkConsistency x code secret) space

-- check equality of a list, need to change to universal
allIdentical :: [Bool] -> Bool
allIdentical xs = and xs

-- generate all repeat permutations
generatePermutations :: [a] -> Int -> [[a]]
generatePermutations set size = replicateM size set

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