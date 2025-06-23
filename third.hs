lineSplitter :: String -> IO [String]
lineSplitter filename = do
  contents <- readFile filename
  return $ lines contents

main :: IO ()
main = do
  putStrLn "A first test. Let's see if it works."
  splitLines <- lineSplitter "default.cabal"
  mapM_ putStrLn splitLines
