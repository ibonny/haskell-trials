lineSplitter :: String -> [String]
lineSplitter = lines

main :: IO ()
main = do
  putStrLn "A first test. Let's see if it works."
  contents <- readFile "default.cabal"
  let splitLines = lineSplitter contents
  mapM_ putStrLn splitLines
