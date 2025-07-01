readFasta :: FilePath -> IO [String]
readFasta filename = do
  contents <- readFile filename
  return $ filter (\l -> head l /= '>') (lines contents)

findIndices :: String -> String -> [Int]
findIndices needle haystack = findWithIndex needle haystack 0
  where
    findWithIndex [] _ _ = []
    findWithIndex _ [] _ = []
    findWithIndex (n : ns) (h : hs) idx
      | n == h = idx : findWithIndex ns hs (idx + 1)
      | otherwise = findWithIndex (n : ns) hs (idx + 1)

main :: IO ()
main = do
  lines' <- readFasta "rosalind_sseq.fasta"

  let matches = [x + 1 | x <- findIndices (lines' !! 1) (head lines')]

  putStrLn $ unwords $ map show matches