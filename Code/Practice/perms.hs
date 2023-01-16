perms :: [[Int]] -> [[Int]]
perms list = case list of
    ([]) -> perms ([0,0,0,1] : list)
    ([0,0,0,0]:tail) -> list
    (head:tail) -> perms (f head : list)
                    where 
                        f [a,b,c,d]
                            | d < 5 = [a,b,c,d+1]
                            | c < 5 = [a,b,c+1,0]
                            | b < 5 = [a,b+1,0,0]
                            | a < 5 = [a+1,0,0,0]
                            | otherwise = [0,0,0,0]