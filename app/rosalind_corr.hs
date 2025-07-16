module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S

readFastaStrings :: FilePath -> IO [String]
readFastaStrings filename = do
  contents <- readFile filename
  let lines' = lines contents
  let lines'' = [x | x <- lines', head x /= '>']
  return lines''

generateReverseComplement :: String -> String
generateReverseComplement = reverse . generateComplement

generateComplement :: String -> String
generateComplement = map complementBase
  where
    complementBase 'A' = 'T'
    complementBase 'T' = 'A'
    complementBase 'C' = 'G'
    complementBase 'G' = 'C'
    complementBase x = x

-- Returns True if two strings differ by exactly one character
isOneOff :: String -> String -> Bool
isOneOff a b = length a == length b && (countDiffs a b == 1)
  where
    countDiffs xs ys = length [() | (x, y) <- zip xs ys, x /= y]

-- -- Given a list, returns all unique unordered pairs that are one base off
-- oneOffPairs :: [String] -> [(String, String)]
-- oneOffPairs xs = [(x, y) | (i, x) <- indexed, (j, y) <- indexed, i < j, isOneOff x y]
--   where
--     indexed :: [(Int, String)]
--     indexed = zip [0 ..] xs

-- Count occurrences of each string in the list
countOccurrences :: [String] -> M.Map String Int
countOccurrences xs = M.fromListWith (+) [(x, 1) | x <- xs]

-- Find corrections for singletons
findCorrections :: [String] -> [(String, String)]
findCorrections xs =
  [ (x, y)
    | x <- xs,
      M.findWithDefault 0 x counts == 1,
      y <- xs,
      x /= y,
      M.findWithDefault 0 y counts > 1 || M.findWithDefault 0 (generateReverseComplement y) counts > 1,
      isOneOff x y
  ]
  where
    counts = countOccurrences xs

main :: IO ()
main = do
  fastaStrings <- readFastaStrings "rosalind_corr.fasta"

  -- let allStrings = concatMap (\x -> [x, generateReverseComplement x]) fastaStrings

  -- -- let pairs = oneOffPairs fastaStrings

  -- -- mapM_ print pairs

  -- let corrections = findCorrections allStrings

  -- mapM_ (\(a, b) -> putStrLn $ a ++ "->" ++ b) corrections

  let counts = countOccurrences fastaStrings
      -- A string is correct if it or its reverse complement appears more than once
      isCorrect x = let rc = generateReverseComplement x
                    in M.findWithDefault 0 x counts > 1 || M.findWithDefault 0 rc counts > 1
      singletons = [x | x <- fastaStrings, M.findWithDefault 0 x counts == 1]
      originals = S.fromList fastaStrings
      -- For each singleton, try every original as a correction candidate
      corrections =
        [ (x, y)
        | x <- singletons
        , y <- S.toList originals
        , isCorrect y
        , isOneOff x y || isOneOff x (generateReverseComplement y)
        ]
      uniqueCorrections = M.toList $ M.fromList corrections

  mapM_ (\(a, b) -> putStrLn $ a ++ "->" ++ b) uniqueCorrections
