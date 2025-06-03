module Main where

myFunc :: String
myFunc = "This is a test."

powerUp :: Int -> Int -> Int
powerUp base ex = foldr (*) 1 (replicate ex base)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    putStrLn myFunc
    print $ powerUp 3 2