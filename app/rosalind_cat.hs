module Main (main) where

import Data.Array
import Data.List (foldl')

readFasta :: FilePath -> IO String
readFasta filename = do
  contents <- readFile filename
  let lines' = lines contents
  let lines'' = [x | x <- lines', head x /= '>']
  let joined = concat lines''
  return joined

-- catalan :: Int -> Integer
-- catalan n = factorial (2 * n) `div` (factorial (n + 1) * factorial n)
--   where
--     factorial k = foldl' (*) 1 [1 .. fromIntegral k]

isPair :: Char -> Char -> Bool
isPair 'A' 'U' = True
isPair 'U' 'A' = True
isPair 'C' 'G' = True
isPair 'G' 'C' = True
isPair _ _ = False

countMatchings :: String -> Integer
countMatchings s = dp ! (0, n - 1)
  where
    n = length s
    bounds = ((0, -1), (n, n - 1))
    dp = array bounds [((i, j), f i j) | i <- [0 .. n], j <- [-1 .. n - 1]]
    f i j
      | i > j = 1
      | otherwise =
        sum
          [ dp ! (i + 1, k - 1) * dp ! (k + 1, j)
            | k <- [i + 1 .. j],
              k < n,
              isPair (s !! i) (s !! k)
          ]
          `mod` 1000000

main :: IO ()
main = do
  line <- readFasta "rosalind_cat.fasta"
  -- print line

  -- print $ length line

  -- let n = length line `div` 2
  -- print $ catalan n `mod` 1000000

  print line

  print $ countMatchings line
