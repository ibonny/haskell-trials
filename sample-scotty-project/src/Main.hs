{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Text.Lazy (Text, pack)
import GHC.Generics (Generic)
import Web.Scotty

-- For JSON data
newtype Message = Message {
    content :: String
} deriving (Generic, Show)

instance ToJSON Message

instance FromJSON Message

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci :: Int -> Integer
fibonacci n = fibs !! n

main :: IO ()
main = scotty 3000 $ do
  -- Home route
  get "/" $ do
    html "<h1>Welcome to our Awesome App!</h1>"

  -- JSON response
  get "/api/data" $ do
    json $
      object
        [ "message" .= ("Hello from Haskell!" :: Text),
          "status" .= (200 :: Int)
        ]

  -- Dynamic route with parameter
  get "/greet/:name" $ do
    name <- param "name"
    text $ "Hello, " <> name <> "!"

  post "/submit/json" $ do
    message <- jsonData :: ActionM Message
    liftIO $ Prelude.putStrLn $ "Received: " ++ show message -- Changed to show message
    json message

  get "/fibonacci" $ do
    let fib10 = fibonacci 100
    text $ pack ("Fibonacci 100 = " ++ show fib10)
