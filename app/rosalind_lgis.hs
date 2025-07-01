import Data.List.Split
import Text.Printf
import Data.List (maximumBy)
import Data.Ord (comparing)

readInput :: FilePath -> IO [String]
readInput filename = tail . lines <$> readFile filename

stringToNumbers :: [String] -> [Int]
stringToNumbers = map read

longestIncreasing :: [Int] -> [Int]
longestIncreasing [] = []
longestIncreasing (x:xs) = 
    let rest = longestIncreasing xs
        withX = x : [y | y <- rest, y > x]
        withoutX = rest
    in if length withX >= length withoutX
       then withX
       else withoutX

longestDecreasing :: [Int] -> [Int]
longestDecreasing [] = []
longestDecreasing xs = 
    let allSequences = [buildSequence x (dropWhile (>= x) xs) | x <- xs]
    in maximumBy (comparing length) allSequences
    where
        buildSequence x [] = [x]
        buildSequence x (y:ys)
            | y < x = x : buildSequence y ys
            | otherwise = buildSequence x ys

main :: IO ()
main = do
  contents <- readInput "rosalind_lgis.txt"

  let entries = stringToNumbers $ splitOn " " (head contents)

  mapM_ (printf "%d ") (longestIncreasing entries)

  putStrLn ""

  mapM_ (printf "%d ") (longestDecreasing entries)

  putStrLn ""

  -- print (longestDecreasing entries)

  -- print (longestIncreasing entries)
