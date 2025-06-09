module Main where

import MyLib

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    putStrLn myFunc
    print $ powerUp 3 2