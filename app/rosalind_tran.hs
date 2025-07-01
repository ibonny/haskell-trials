-- Filter a FASTA file by its header, and then concat the rest of the lines together.
-- Not sure why the last line is required for completeness.
getSequences :: [String] -> [String]
getSequences [] = []
getSequences (('>' : _) : rest) = getSequences rest
getSequences (x : y : rest) = (x ++ y) : getSequences rest
getSequences rest = rest

compareStrings :: String -> String -> [Int]
compareStrings [] _ = []
compareStrings _ [] = []
compareStrings (x : xs) (y : ys)
  | x == 'A' && y == 'G' = 0 : compareStrings xs ys
  | x == 'C' && y == 'T' = 0 : compareStrings xs ys

  | x == 'A' && y == 'A' = compareStrings xs ys
  | x == 'C' && y == 'C' = compareStrings xs ys
  | x == 'G' && y == 'G' = compareStrings xs ys
  | x == 'T' && y == 'T' = compareStrings xs ys

  | otherwise = 1 : compareStrings xs ys

countEntries :: [Int] -> Int -> Int
countEntries [] _ = 0
countEntries (x:xs) val
  | x == val = 1 + countEntries xs val
  | otherwise = 0 + countEntries xs val

main :: IO ()
main = do
  content <- readFile "rosalind_tran.fasta"

  let sequences = getSequences (words content)

  putStrLn $ head sequences
  putStrLn $ sequences !! 1

  let diff = compareStrings (head sequences) (sequences !! 1)

  let transitions = fromIntegral (countEntries diff 0) :: Float
  let transversions = fromIntegral (countEntries diff 1) :: Float

  print transitions
  print transversions

  print $ transitions / transversions

  print diff