module MyLib where

myFunc :: String
myFunc = "This is a test."

powerUp :: Int -> Int -> Int
powerUp base ex = product (replicate ex base)
