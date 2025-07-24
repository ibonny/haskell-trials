module Main (main) where

import Text.Printf (printf)

numberOfInternalNodes :: Int -> Int
numberOfInternalNodes n = n - 2

main :: IO ()
main = printf "The number of internal nodes on a tree of 4 nodes is: %d\n" $ numberOfInternalNodes 4
