-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 15/01/2023
-- Author: Kyle Dick, kd41@hw.ac.uk

import Data.List

{-
This base solution is the Brute Solution.
It simply takes a list of previous guesses and finds
the next in the sequence of all 1296 possible combinations.
Once the correct one is find, the number of steps taken to
arrive is given.

This implementation has some obvious problems, one of which
is that the efficiency is dependant on the input itself -
[0,0,0,1] will be solved a long time before [5,5,5,5] is solved.

The structure of the program makes the next step clear however.
The data structure of previous guesses retains the evaluations
given which allows for the refining of the exhaustive process
by narrowing the search space that will be iterated through.

A reassessment of the guesses data structure is also required,
the form of key:value pairings could be restructured as a dict
but for this process and array was sufficient as the first element
is important.
-}

bruteSolution :: [a] -> [([a], (Int,Int))] -> [a]
bruteSolution s guesses = case guesses of
    [] -> replicate 4 (head s)
    (x:xs) -> f $ fst x     -- first tuple elem is the code
        where
            f [a,b,c,d]
                | d < 5 = [a,b,c,d+1]
                | c < 5 = [a,b,c+1,0]
                | b < 5 = [a,b+1,0,0]
                | a < 5 = [a+1,0,0,0]
                | otherwise = [0,0,0,0]
    
{-
This function evaluates a code based on the hidden combination
If given two lists, it will return a tuple which evaluates the
number of correct elements based on appearance in the first tuple element
and correct element AND position in the second tuple element.
-}
checkGuess :: Eq a => [a] -> [a] -> (Int,Int)
checkGuess code guess = foldl assignPegs (0,0) (zip code guess)
    where 
    assignPegs (white, black) (x, y)
        | x == y = (white, black + 1)
        | x `elem` guess = (white + 1, black)
        | otherwise = (white, black)

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