import qualified Data.Text as T

getRosalindLines :: FilePath -> IO [String]
getRosalindLines filename = do
  contents <- readFile filename
  let lines' = lines contents
  let lines'' = filter (\l -> head l /= '>') lines'
  return lines''

removeIntrons :: [String] -> String -> String
removeIntrons stringsToRemove input = 
    T.unpack $ foldl (\acc str -> T.replace (T.pack str) (T.pack "") acc) (T.pack input) stringsToRemove

main :: IO ()
main = do
  rosalindLines <- getRosalindLines "rosalind_splc.fasta"

  let newLine = removeIntrons (tail rosalindLines) (head rosalindLines)

  putStrLn newLine