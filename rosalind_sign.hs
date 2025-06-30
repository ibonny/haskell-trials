module Main (main) where

import Data.List (permutations)
import Text.Printf (printf)

doPrint :: [[Int]]
doPrint input = do
  [ printf "%d" x :: String | x <- input ]

main :: IO ()
main = do
  let permNum = 4

  let chooseList = [-permNum .. permNum]

  let perms = permutations chooseList

  let len = length perms

  doPrint perms

  print ""
