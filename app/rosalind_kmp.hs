module Main (main) where

-- -- Reading in the fasta file
-- readFastaFile :: FilePath -> IO String
-- readFastaFile filename = do
--   content <- readFile filename

--   let lines' = lines content

--   let lines'' = filter (\x -> head x /= '>') lines'

--   return $ concat lines''

-- -- Get all prefixes of a string
-- getPrefixes :: String -> [String]
-- getPrefixes str = [take i str | i <- [1 .. length str]]

-- -- Get all possible substrings ending at position k, starting from j=2
-- getSubstringsEndingAt :: String -> Int -> [String]
-- getSubstringsEndingAt str k =
--   [drop j (take k str) | j <- [1 .. k -1]]

-- -- Find length of longest matching prefix for position k
-- findLongestMatch :: String -> Int -> Int
-- findLongestMatch str k =
--   let prefixes = getPrefixes str
--       substrings = getSubstringsEndingAt str k
--       matches = [length s | s <- substrings, s `elem` prefixes]
--    in if null matches then 0 else maximum matches

-- -- Build the failure array
-- buildFailureArray :: String -> [Int]
-- buildFailureArray str =
--   0 : [findLongestMatch str k | k <- [2 .. length str]]

-- -- Example usage:
-- main :: IO ()
-- main = do
--   contents <- readFastaFile "rosalind_kmp.fasta"

--   let result = buildFailureArray contents

--   putStrLn $ unwords $ map show result

readFastaFile :: FilePath -> IO String
readFastaFile filename = do
  content <- readFile filename
  return $ concat $ filter (\x -> head x /= '>') $ lines content

-- Optimized failure array computation using KMP approach
buildFailureArrayFast :: String -> [Int]
buildFailureArrayFast str =
  let n = length str
      compute = go 1 0
      go i len
        | i >= n = []
        | str !! i == str !! len = (len + 1) : go (i + 1) (len + 1)
        | len > 0 = go i (p !! (len - 1))
        | otherwise = 0 : go (i + 1) 0
      p = 0 : compute
   in p

main :: IO ()
main = do
  contents <- readFastaFile "rosalind_kmp.fasta"
  let result = buildFailureArrayFast contents
  putStrLn $ unwords $ map show result
