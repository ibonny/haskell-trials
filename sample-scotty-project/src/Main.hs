{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Text.Lazy (Text)
import Data.Aeson (object, (.=), FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)

-- For JSON data
newtype Message = Message
    { content :: String
    } deriving (Generic, Show)

instance ToJSON Message
instance FromJSON Message

main :: IO ()
main = scotty 3000 $ do
    -- Home route
    get "/" $ do
        html "<h1>Welcome to our Awesome App!</h1>"

    -- JSON response
    get "/api/data" $ do
        json $ object [ "message" .= ("Hello from Haskell!" :: Text)
                     , "status" .= (200 :: Int)
                     ]

    -- Dynamic route with parameter
    get "/greet/:name" $ do
        name <- param "name"
        text $ "Hello, " <> name <> "!"

    post "/submit/json" $ do
        message <- jsonData :: ActionM Message
        liftIO $ Prelude.putStrLn $ "Received: " ++ show message  -- Changed to show message
        json message