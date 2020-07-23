module Main where

import Lib

main :: IO ()
main = do 
    text <- readFile "./resources/sirens_fxt.txt" 
    let sirenList = sirenTextToList text
    -- putStrLn $ show $ length sirenList
    putStrLn $ show $ countUniqueAndNonUnique sirenList
    -- putStrLn $ show $ cuanuNaive sirenList

{- 3 sirens that do not have the right format in sirens_fxt.txt 
03894407
0389440A4
0389440621
-}
