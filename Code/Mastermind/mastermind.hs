-- Basic Implementation of Mastermind in Haskell
-- Date of Creation: 17/10/2022
-- Date of Last Edit: 17/10/2022
-- Author: Kyle Dick, kd41@hw.ac.uk

-- define parameters for the board
maxGuess, codeLength :: Integer
setSize = 6     -- code contains integers from 0 to setSize-1
maxGuess = 10
codeLength = 4

{-
input:
     Int - Size of Elements in Set,
     Int - Length of Desired Code,
-}
generateCode :: Int -> Int -> Int
generateCode set length =
    case length of
        0 -> -1
        _ -> 10^(length-1)