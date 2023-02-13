-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 15/01/2023
-- Author: Kyle Dick, kd41@hw.ac.uk
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.List
import Distribution.Compat.CharParsing (space)
import Control.Monad (replicateM)
import Data.Data (gcast2)
import System.Win32 (xBUTTON1)

{-----------------------------
            TESTING
------------------------------}

-- functions for use in testing using the interactive environment
testSet :: Num a => [([a],(Int,Int))]
testSet = [([4,1,2,3], (2,2)) , ([0,2,3,4], (0,3)), ([1,0,0,0], (0,1))]
testSpace :: Num a => [[a]]
testSpace = [[1,2,3,3],[3,0,3,2],[0,3,2,3],[3,0,1,3],[4,4,4,4],[1,0,4,0]]

{-----------------------------
            SET UP
------------------------------}

{-----------------------------
    MASTERMIND FUNCTIONS
------------------------------}

checkWhite :: Eq a => [a] -> [a] -> Int -> Int
checkWhite g1 g2 count = case elemIndex (head g1) g2 of
    Just n -> do 
        if length g1 == 1 then count + 1
        else checkWhite (drop 1 g1) (removeIndex g2 n) (count + 1)
    Nothing -> 
        if length g1 == 1 then count
        else checkWhite (drop 1 g1) g2 count

checkBlack :: Eq a => [a] -> [a] -> [Int]
checkBlack g1 g2 = elemIndices True (zipWith (==) g1 g2)

evalGuess :: Eq a => [a] -> [a] -> (Int,Int)
evalGuess g1 g2 = 
    do
        let black = checkBlack g1 g2
        if length black == 4 then (4,0)
        else (length black,
         checkWhite (removeIndices g1 black) (removeIndices g2 black) 0)

-- this function returns the next guess at the current step
-- inputs the secret code, the previous guesses and all possible answers at the current step
-- solveStep :: Eq a => [a] -> [([a], (Int,Int))] -> [[a]] -> [a]
-- solveStep secret guesses space = head (pruneInconsistent (fst $ head guesses) secret space)

-- mastermind :: Eq a => [a] -> [a] -> [([a], (Int,Int))] -> [([a], (Int,Int))]
-- mastermind symbols code guesses = 
--     let guess = solveStep symbols guesses
--         check = checkGuess code guess
--     in if check == (0,4)
--         then (guess, (0,4)) : guesses
--         else mastermind symbols code ((guess,check) : guesses)


{-----------------------------
    CONSISTENCY FUNCTIONS
------------------------------}

-- -- function to evaluate if a guess is consistent with another with respect to the secret code
-- checkConsistency :: Eq a => [a] -> [a] -> [a] -> Bool
-- checkConsistency guess code secret = evalGuess guess code == evalGuess code secret

-- -- function which checks if a new guess is consistent with all previous guesses with respect to the secret code
-- checkConsistencySet :: Eq a => [a] -> [([a], (Int,Int))] -> [a] -> [Bool]
-- checkConsistencySet code consistentSet secret = let conCodes = map (\(list,_) -> map (\x -> x) list) consistentSet
--  in map (\x -> evalGuess code x == evalGuess x secret) conCodes

{-----------------------------
    COMBINATORIAL FUNCTIONS
------------------------------}

-- -- removes all inconsistent 
-- pruneInconsistent :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
-- pruneInconsistent code secret space = filter (\x -> checkConsistency x code secret) space

-- generate all repeat permutations
generatePermutations :: [a] -> Int -> [[a]]
generatePermutations set size = replicateM size set

{-----------------------------
    UTILITY FUNCTIONS
------------------------------}

-- check equality of a list, need to change to universal
allIdentical :: [Bool] -> Bool
allIdentical xs = and xs

removeIndex :: [a] -> Int -> [a]
removeIndex list i = let (xs,ys) = splitAt i list in
    xs ++ drop 1 ys

removeIndices :: [a] -> [Int] -> [a]
removeIndices list indices = if not $ null indices
        then do
            let newList = removeIndex list $ head indices
            removeIndices newList (map (\x -> x - 1) (drop 1 indices))
        else
            list

-- main :: IO()
-- main = do 
--     let x = mastermind ['A','B','C','D','E','F'] ['A','C','D','E'] []
--         in print (length x)