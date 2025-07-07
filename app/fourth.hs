import qualified Data.ByteString.Char8 as B
import Network.HTTP.Simple
import Text.Printf (printf)

forLoopTest :: Int -> IO ()
forLoopTest 5 = return ()
forLoopTest val = do
  printf "Doing this step %d times.\n" val
  forLoopTest (val + 1)

repeatNTimes :: (Monad m) => Int -> m a -> m ()
repeatNTimes 0 _ = return ()
repeatNTimes n action = do
  _ <- action
  repeatNTimes (n - 1) action

getLinesFromFile :: FilePath -> IO [String]
getLinesFromFile filename = do
  contents <- readFile filename
  return $ lines contents

getDataFromWebsite :: String -> IO B.ByteString
getDataFromWebsite url = do
  request <- parseRequest url
  response <- httpBS request
  return $ getResponseBody response

stripCR :: String -> String
stripCR = filter (/= '\r')

main :: IO ()
main = do
  -- putStrLn "This is a test. I hope it goes well"

  -- forLoopTest 0

  -- repeatNTimes 5 (putStrLn "Ok, this does work."

  -- lines' <- getLinesFromFile "default.cabal"

  -- mapM_ putStrLn lines'

  content <- getDataFromWebsite "https://jsonplaceholder.typicode.com/posts"

  putStrLn $ stripCR $ B.unpack content
