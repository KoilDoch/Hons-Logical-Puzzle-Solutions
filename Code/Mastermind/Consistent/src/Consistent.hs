-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 15/01/2023
-- Author: Kyle Dick, kd41@hw.ac.uk
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.List
import Test.QuickCheck
import Control.Monad (replicateM)
import Data.Data (gcast2)
import GHC.StgToCmm.ExtCode (code)
import GHC.Plugins (space)

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

checkWhite :: Eq a => [a] -> [a] -> [a] -> Int
checkWhite [] _ _ = 0
checkWhite _ [] _ = 0
checkWhite _ _ [] = 0
checkWhite (s:symbols) ys zs = min (countOccurences s ys) (countOccurences s ys) + checkWhite symbols ys zs

checkBlack :: Eq a => [a] -> [a] -> Int
checkBlack _ [] = 0
checkBlack [] _ = 0
checkBlack (x:xs) (y:ys) = if x == y then 1 + checkBlack xs ys else checkBlack xs ys

evalGuess :: Eq a => [a] -> [a] -> [a] -> (Int, Int)
evalGuess symbols xs ys = let black = checkBlack xs ys 
                    in (black, checkWhite symbols xs ys - black)

-- this function returns the next guess at the current step
-- inputs the secret code, the previous guesses and all possible answers at the current step
solveStep :: Eq a => [a] -> [([a], (Int,Int))] -> [[a]] -> [a]
solveStep secret guesses space = 
    do
        let newSpace = pruneInconsistent (fst $ head guesses) secret space
        let nextStep = head newSpace
        if allIdentical $ checkConsistencySet nextStep guesses secret
            then nextStep
            else solveStep secret guesses $ drop 1 newSpace

isSpace :: [a] -> Int -> [[a]] -> [[a]]
isSpace symbols i [] = generatePermutations symbols i
isSpace symbols i s = s

isGuesses :: Eq a => [[a]] -> [a] -> [([a], (Int,Int))] -> [([a], (Int,Int))]
isGuesses space secret [] = [(head space, evalGuess (head space) secret)]
isGuesses space secret guesses = guesses

mastermind :: Eq a => [a] -> [a] -> [([a], (Int,Int))] -> [[a]] -> [([a], (Int,Int))]
mastermind symbols secret [] [] = 
    do
        let space = generatePermutations symbols $ length secret
        let guesses = [(head space, evalGuess (head space) secret)]
        let newGuess = solveStep secret guesses space
        if evalGuess newGuess secret == (4,0)
            then
                (newGuess, evalGuess newGuess secret) : guesses
            else mastermind symbols secret ((newGuess, evalGuess newGuess secret) : guesses) space
            
mastermind symbols secret guesses space =
    do
        let newGuess = solveStep secret guesses space
        if evalGuess newGuess secret == (4,0)
            then
                (newGuess, evalGuess newGuess secret) : guesses
            else mastermind symbols secret ((newGuess, evalGuess newGuess secret) : guesses) space 

{-----------------------------
    CONSISTENCY FUNCTIONS
------------------------------}

-- -- function to evaluate if a guess is consistent with another with respect to the secret code
checkConsistency :: Eq a => [a] -> [a] -> [a] -> Bool
checkConsistency guess code secret = evalGuess guess code == evalGuess code secret

-- -- function which checks if a new guess is consistent with all previous guesses with respect to the secret code
checkConsistencySet :: Eq a => [a] -> [([a], (Int,Int))] -> [a] -> [Bool]
checkConsistencySet code consistentSet secret = let conCodes = map (\(list,_) -> map (\x -> x) list) consistentSet
 in map (\x -> evalGuess code x == evalGuess x secret) conCodes

{-----------------------------
    COMBINATORIAL FUNCTIONS
------------------------------}

-- -- removes all inconsistent 
pruneInconsistent :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
pruneInconsistent code secret space = filter (\x -> checkConsistency x code secret) space

-- generate all repeat permutations
generatePermutations :: [a] -> Int -> [[a]]
generatePermutations set size = replicateM size set
--}
{-----------------------------
    UTILITY FUNCTIONS
------------------------------}

countOccurences :: Eq a => a -> [a] -> Int
countOccurences _ [] = 0
countOccurences x ys = length $ filter (x==) ys

-- check equality of a list, need to change to universal
allIdentical :: [Bool] -> Bool
allIdentical = and

removeIndex :: [a] -> Int -> [a]
removeIndex list i = let (xs,ys) = splitAt i list in
    xs ++ drop 1 ys

removeIndices :: [a] -> [Int] -> [a]
removeIndices list [] = list
removeIndices [] _ = []
removeIndices list indices = 
    do
        let newList = removeIndex list $ head indices
        removeIndices newList (map (\x -> x - 1) (drop 1 indices))