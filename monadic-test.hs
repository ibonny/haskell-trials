import Network.HTTP.Simple
import Data.ByteString.Lazy.Char8 as L8

newtype HttpM a = HttpM { runHttp :: IO a }

instance Functor HttpM where
    fmap f (HttpM io) = HttpM (fmap f io)

instance Applicative HttpM where
    pure x = HttpM (pure x)
    HttpM f <*> HttpM x = HttpM (f <*> x)

instance Monad HttpM where
    return = pure
    HttpM io >>= f = HttpM $ io >>= (runHttp . f)

get :: String -> HttpM ByteString
get url = HttpM $ do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

-- Usage in main:
main = do
    result <- runHttp $ do
        response <- get "https://jsonplaceholder.typicode.com/users"
        return $ L8.unpack response

    print result
