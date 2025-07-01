{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import GHC.Generics
import Data.Aeson
import Data.Char (toUpper)

data User = User {
    id :: Int,
    name :: String,
    username :: String,
    email :: String
} deriving (Show, Generic)

instance FromJSON User where
    parseJSON = genericParseJSON defaultOptions

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "GET https://jsonplaceholder.typicode.com/users"
    response <- httpLbs request manager
    let users = decode (responseBody response) :: Maybe [User]
    case users of
        Just u -> mapM_ (putStrLn . map toUpper . name) u
        Nothing -> putStrLn "Failed to parse JSON"
