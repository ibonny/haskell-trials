{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.HTTP.Simple

-- import Text.Printf (printf)

data Post = Post
  { userId :: Int,
    postId :: Int,
    title :: String,
    body :: String
  }
  deriving (Show, Generic)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \v ->
    Post
      <$> v .: "userId"
      <*> v .: "id"
      <*> v .: "title"
      <*> v .: "body"

-- forLoopTest :: Int -> IO ()
-- forLoopTest 5 = return ()
-- forLoopTest val = do
--   printf "Doing this step %d times.\n" val
--   forLoopTest (val + 1)

-- repeatNTimes :: (Monad m) => Int -> m a -> m ()
-- repeatNTimes 0 _ = return ()
-- repeatNTimes n action = do
--   _ <- action
--   repeatNTimes (n - 1) action

-- getLinesFromFile :: FilePath -> IO [String]
-- getLinesFromFile filename = do
--   contents <- readFile filename
--   return $ lines contents

getDataFromWebsite :: String -> IO B.ByteString
getDataFromWebsite url = do
  request <- parseRequest url
  response <- httpBS request
  return $ getResponseBody response

-- getDataFromWebsite :: String -> IO B.ByteString
-- getDataFromWebsite url = do
--   request <- parseRequest url
--   response <- httpBS request
--   return $ getResponseBody response

getPostsFromWebsite :: String -> IO [Post]
getPostsFromWebsite url = do
  response <- getDataFromWebsite url
  let maybePosts = decode (B.fromStrict response) :: Maybe [Post]
  return $ fromMaybe [] maybePosts

-- stripCR :: String -> String
-- stripCR = filter (/= '\r')

main :: IO ()
main = do
  -- putStrLn "This is a test. I hope it goes well"

  -- forLoopTest 0

  -- repeatNTimes 5 (putStrLn "Ok, this does work."

  -- lines' <- getLinesFromFile "default.cabal"

  -- mapM_ putStrLn lines'

  content <- getPostsFromWebsite "https://jsonplaceholder.typicode.com/posts"

  mapM_ (\l -> putStrLn $ show (postId l) ++ ": " ++ title l) content
