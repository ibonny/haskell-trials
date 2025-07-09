{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Generics
import Data.Aeson (eitherDecode, FromJSON)
import Network.HTTP.Simple
-- import qualified Data.ByteString.Lazy.Char8 as L

data Post = Post
    { userId :: Int
    , id :: Int
    , title :: String
    , body :: String
    } deriving (Show, Generic)

instance FromJSON Post

-- Simple GET request
makeGetRequest :: String -> IO (Either String [Post])
makeGetRequest url = do
    request <- parseRequest url
    response <- httpLBS request
    -- putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    -- return $ L.unpack (getResponseBody response)
    return $ eitherDecode (getResponseBody response)

main :: IO ()
main = do
    -- result <- makeGetRequest "https://jsonplaceholder.typicode.com/posts/1"

    -- putStrLn result

    result <- makeGetRequest "https://jsonplaceholder.typicode.com/posts"

    case result of
        Right posts -> mapM_ (print . title) posts -- (take 2 posts)  -- Show first 2 posts
        Left err -> print err
