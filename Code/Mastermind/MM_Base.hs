-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 15/01/2023
-- Author: Kyle Dick, kd41@hw.ac.uk

-- takes the previous guesses, returns a guess based on that
bruteSolution :: [([Int], (Int, Int))] -> [Int]
bruteSolution guesses = case guesses of
    [] -> [0,0,0,0]
    (x:xs) -> f $ fst x
        where
            f [a,b,c,d]
                | d < 5 = [a,b,c,d+1]
                | c < 5 = [a,b,c+1,0]
                | b < 5 = [a,b+1,0,0]
                | a < 5 = [a+1,0,0,0]
                | otherwise = [0,0,0,0]

-- takes the code and the list of previous guesses and tries the solution on each
mastermind :: [Int] -> [([Int], (Int, Int))] -> [([Int], (Int, Int))]
mastermind code guesses = 
    let guess = bruteSolution guesses
        check = checkGuess code guess
    in if check == (0,4)
        then (guess, (0,4)) : guesses
        else mastermind code ((guess,check) : guesses)
    
-- This function evaluates a code based on the hidden combination
-- If given two lists, it will return a tuple which evaluates the
--  number of correct elements based on appearance in the first tuple element
--  and correct element AND position in the second tuple element.
checkGuess :: Eq code => [code] -> [code] -> (Int, Int)
checkGuess code guess = foldl f (0,0) (zip code guess)
    where 
    f (white, black) (x, y)
        | x == y = (white, black + 1)
        | x `elem` guess = (white + 1, black)
        | otherwise = (white, black)

main :: IO()
main = do 
    let x = mastermind [0,0,4,0] []
        in print (length x)