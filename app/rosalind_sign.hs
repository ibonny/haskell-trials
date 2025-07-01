import Data.List (permutations, subsequences)

toString :: [Int] -> String
toString xs = unwords (map show xs)

-- Get all permutations of length k
kPermutations :: Int -> [a] -> [[a]]
kPermutations k xs = filter ((== k) . length) $ subsequences xs >>= permutations

main :: IO ()
main = do
  let permNum = 5

  let chooseList = filter (/= 0) [-permNum .. permNum]

  let perms = kPermutations permNum chooseList

  let perms' = filter (\l -> abs (head l) /= abs (l !! 1)) perms

  let len = length perms'

  print len

  mapM_ (putStrLn . toString) perms'
