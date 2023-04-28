import GHC (guessTarget)
import GHC.Cmm (sectionProtection)
import Control.Monad hiding (forever)
import Test.QuickCheck (generate)
import GHC.Plugins (space)
{---

    RESPONSE FUNCTIONS

---}

checkWhite :: Eq a => [a] -> [a] -> [a] -> Int
checkWhite [] _ _ = 0
checkWhite _ [] _ = 0
checkWhite _ _ [] = 0
checkWhite (s:symbols) code guess = checkWhite symbols code guess + min (countOccurences s code) (countOccurences s guess)

checkBlack :: Eq a => [a] -> [a] -> Int
checkBlack _ [] = 0
checkBlack [] _ = 0
checkBlack (x:code) (y:guess) = if x == y 
                                    then 1 + checkBlack code guess 
                                    else checkBlack code guess

evalGuess :: Eq a => [a] -> [a] -> [a] -> (Int, Int)
evalGuess symbols code guess = let black = checkBlack code guess 
                                in (black, checkWhite symbols code guess - black)

isSolved :: (Int,Int) -> Bool
isSolved (4, 0) = True
isSolved (_,_) = False

countOccurences :: Eq a => a -> [a] -> Int
countOccurences _ [] = 0
countOccurences x ys = length $ filter (x==) ys

{---

    BASE SOLUTIONS

---}

baseStep :: [Int] -> [Int]
baseStep [] = [0,0,0,0]
baseStep [a,b,c,d]  | a < 5 = [a+1, b, c, d]
                    | b < 5 = [0, b+1, c, d]
                    | c < 5 = [0, 0, c+1, d]
                    | d < 5 = [0, 0, 0, d+1]
                    | otherwise = [0,0,0,0]

-- secret code, symbols, initial guess
baseSolution :: [Int] -> [Int] -> [Int] -> [([Int], (Int,Int))]
baseSolution [] _ _ = []
baseSolution symbols code guess = 
    do
        let response = evalGuess symbols code guess
        if response == (4,0)
            then [(guess, response)]
            else (guess, response) : baseSolution code symbols (baseStep guess)

{---

    CONSISTENT SOLUTION

---}

generatePermutations :: [a] -> Int -> [[a]]
generatePermutations set size = replicateM size set

pruneInconsistent :: Eq a => [a] -> [a] -> [a] -> [[a]] -> [[a]]
pruneInconsistent symbols code secret [] = []
pruneInconsistent symbols code secret (x:space) | consistent = x : pruneInconsistent symbols code secret space
                                                | not consistent = pruneInconsistent symbols code secret space
                                                where consistent = checkConsistency symbols x code secret

checkConsistency :: Eq a => [a] -> [a] -> [a] -> [a] -> Bool
checkConsistency symbols guess code secret = evalGuess symbols guess code == evalGuess symbols code secret

checkConsistencySet :: Eq a => [a] -> [a] -> [([a], (Int,Int))] -> [a] -> Bool
checkConsistencySet symbols guess [] secret = True
checkConsistencySet symbols guess (x:consistentSet) secret | consistent = checkConsistencySet symbols guess consistentSet secret
                                                           | not consistent = False 
                                                           where consistent = checkConsistency symbols guess (fst x) secret

consistentSolution :: Eq a => [a] -> [a] -> [[a]] -> [([a],(Int,Int))] -> [([a],(Int,Int))]
-- no space of possible guesses
consistentSolution symbols code [] [] = consistentSolution symbols code (generatePermutations symbols (length code)) []
-- no previous guess attempts
consistentSolution symbols code (x:space) [] = consistentSolution symbols code space [(x, evalGuess symbols x code)]
-- evaluate the next possible code
consistentSolution symbols code (x:space) (y:consistentSet) 
                                                            -- solution found
                                                            | snd y == (4,0) = y:consistentSet
                                                            -- possible continuation found
                                                            | consistent = if checkConsistencySet symbols x consistentSet code
                                                                            -- continuation consistent with previous guesses
                                                                            then consistentSolution symbols code space ((x, evalGuess symbols x code):y:consistentSet)
                                                                            -- continuation not consistent with previous guesses
                                                                            else consistentSolution symbols code space (y:consistentSet)
                                                            -- possible continuation not found
                                                            | not consistent = consistentSolution symbols code space (y:consistentSet)
                                                            -- consistent = is the next code in the search space consistent with the last guess
                                                            where consistent = evalGuess symbols x (fst y) == snd y