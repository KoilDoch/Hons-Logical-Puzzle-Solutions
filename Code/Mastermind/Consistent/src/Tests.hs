module Tests (test) where

import System.Directory

test :: IO ()
test = do
    contents <- listDirectory "src"
    print contents