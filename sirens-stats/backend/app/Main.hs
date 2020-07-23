module Main where

import Lib

main :: IO ()
main = putStrLn $ show $ countUniqueAndNonUnique [3, 2, 7, 7, 3, 3, 1]
