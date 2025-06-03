-- Memoized fibonacci using infinite lists
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Function to get the nth fibonacci number
fib :: Int -> Integer
fib n = fibs !! n

main :: IO ()
main = do
    putStrLn "Hello, World!"
    putStrLn $ "The 10th Fibonacci number is: " ++ show (fib 100)
