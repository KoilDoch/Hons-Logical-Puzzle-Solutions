-- The factorial function in Haskell
-- Date of Creation: 12/10/2022
-- Date of Last Edit: 12/10/2022
-- Author: Kyle Dick, kd41@hw.ac.uk

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac(n-1)

main = do 
    putStrLn "What is 5! ?" 
    x <- readLn 
    if x == fac 5 
        then putStrLn "Right" 
        else putStrLn "Wrong"