-- P(n,r) = n!/(n-r)!

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

main :: IO ()
main = do
  let value1 = 96 :: Integer
  let value2 = 8 :: Integer

  let fact1 = factorial value1 :: Integer
  let fact2 = factorial (value1 - value2) :: Integer

  -- putStrLn $ show fact1
  -- putStrLn $ show fact2

  let fullValue = div fact1 fact2

  print $ fullValue `mod` 1000000