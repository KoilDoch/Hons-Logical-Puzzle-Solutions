-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 17/10/2022
-- Author: Kyle Dick, kd41@hw.ac.uk

-- define parameters for the board
colours :: [Char]
maxGuess, codeLength :: Integer
colours = "RYBOGP"
maxGuess = 6
codeLength = 4

-- input a code length, output a code of that length
checkGuess :: ([Char], [Char]) -> Bool
checkGuess ([],[]) = True


