fastaContents :: FilePath -> IO [String]
fastaContents filename = do
  contents <- readFile filename
  let lines' = filter (\l -> head l /= '>') (lines contents)
  return lines'

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

countOccurence :: String -> Char -> Integer
countOccurence "" _ = 0
countOccurence (x:xs) c = if x == c then 1 + countOccurence xs c else countOccurence xs c

main :: IO ()
main = do
  entries <- fastaContents "rosalind_pmch.fasta"

  let aOccur = countOccurence (head entries) 'A'
  let cOccur = countOccurence (head entries) 'C'

  let factA = factorial aOccur
  let factC = factorial cOccur

  print $ factA * factC
