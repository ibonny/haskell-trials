module Main (main) where

import Data.List (tails)
import Text.Printf (printf)

-- Constants
bases :: [Char]
bases = ['A', 'C', 'G', 'T']

-- Reading in the fasta file
readFastaFile :: FilePath -> IO String
readFastaFile filename = do
  content <- readFile filename

  let lines' = lines content

  let lines'' = filter (\x -> head x /= '>') lines'

  return $ concat lines''

-- Generate all k-mers of length 4
fourMers :: [String]
fourMers = sequence (replicate 4 bases)

countKmers :: String -> [(String, Int)]
countKmers seq' = [(kmer, length $ filter (== kmer) allKmers) | kmer <- fourMers]
  where
    allKmers = map (take 4) $ tails seq'

main :: IO ()
main = do
  lns <- readFastaFile "rosalind_kmer.fasta"

  mapM_ (\(_, count) -> printf "%d " count) $ countKmers lns

  putStrLn ""
