module Main (main) where

import Data.List.Split
import qualified Data.Text as T
import Text.Printf (printf)

stringToNumbers :: [String] -> [Double]
stringToNumbers = map read

cgContent :: Double -> Double
cgContent x = x / 2

atContent :: Double -> Double
atContent x = (1 - x) / 2

calcProbOfStr :: String -> Double -> Double
calcProbOfStr "" _ = 1
calcProbOfStr (x : xs) p
  | x == 'C' = cgContent p * calcProbOfStr xs p
  | x == 'G' = cgContent p * calcProbOfStr xs p
  | x == 'A' = atContent p * calcProbOfStr xs p
  | otherwise = atContent p * calcProbOfStr xs p

main :: IO ()
main = do
  -- let str = "ACGATACAA"
  -- let probs = "0.129 0.287 0.423 0.476 0.641 0.742 0.783"

  let str = "GCGTTTTTCACTTGTCTGCCTGTCTTTCTAAGAATGGCAACCCGGGCCTCGTAGCCTTCTCAATCGCTATGTTATTGGTCGCTCTTTCTAGAC"
  let probs = "0.079 0.114 0.177 0.209 0.259 0.337 0.381 0.438 0.468 0.544 0.581 0.634 0.668 0.749 0.784 0.837 0.883 0.905"

  let probsArray = stringToNumbers $ splitOn " " probs

  let finalResult = [logBase 10 $ calcProbOfStr str x | x <- probsArray]

  let printableResult = foldl (printf "%s %.3f") "" finalResult

  putStrLn $ T.unpack $ T.strip $ T.pack printableResult
