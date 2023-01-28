-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 15/01/2023
-- Author: Kyle Dick, kd41@hw.ac.uk

import Data.List
import Distribution.Compat.CharParsing (space)

exhaustSol :: ([a], (Int, Int)) -> [([a], (Int,Int))] -> [a]
exhaustSol guess space = foldl assessPerm [] space
    where 
        assessPerm consistent perm
            | fst guess > snd $ fst perm = consistent : [perm]
    
{-
This function evaluates a code based on the hidden combination
If given two lists, it will return a tuple which evaluates the
number of correct elements based on appearance in the first tuple element
and correct element AND position in the second tuple element.
-}
checkGuess :: Eq a => [a] -> [a] -> (Int,Int)
checkGuess code guess = foldl assignPegs (0,0) (zip code guess)
    where 
    assignPegs (black, white) (x, y)
        | x == y = (black, white + 1)
        | x `elem` guess = (black + 1, white)
        | otherwise = (black, white)

-- takes the code and the list of previous guesses and tries the solution on each
mastermind :: Eq a => [a] -> [a] -> [([a], (Int,Int))] -> [([a], (Int,Int))]
mastermind symbols code guesses = 
    let guess = bruteSolution symbols guesses
        check = checkGuess code guess
    in if check == (0,4)
        then (guess, (0,4)) : guesses
        else mastermind symbols code ((guess,check) : guesses)

main :: IO()
main = do 
    let x = mastermind ['A','B','C','D','E','F'] ['A','C','D','E'] []
        in print (length x)