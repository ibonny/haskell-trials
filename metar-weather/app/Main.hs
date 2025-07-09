{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Generics
import Data.Aeson (eitherDecode, FromJSON, parseJSON)
import Network.HTTP.Simple
import System.Environment
import System.Exit
import Control.Monad
import Control.Applicative
import Text.Printf (printf)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.Clock
import Data.Time.LocalTime

-- https://aviationweather.gov/api/data/metar?ids=KMCI&format=json

-- [{"metar_id":792602187,"icaoId":"KTPA","receiptTime":"2025-07-09 18:57:17","obsTime":1752087180,"reportTime":"2025-07-09 19:00:00","temp":32.2,"dewp":23.9,"wdir":230,"wspd":6,
-- "wgst":null,"visib":7,"altim":1019.4,"slp":1019.3,"qcField":5,"wxString":null,"presTend":null,"maxT":null,"minT":null,"maxT24":null,"minT24":null,"precip":null,"pcp3hr":null,
-- "pcp6hr":null,"pcp24hr":null,"snow":null,"vertVis":null,"metarType":"METAR","rawOb":"KTPA 091853Z COR 23006KT 7SM FEW035 SCT050 SCT250 32\/24 A3010 RMK AO2 SLP193 T03220239",
-- "mostRecent":1,"lat":27.9633,"lon":-82.54,"elev":2,"prior":2,"name":"Tampa Intl Arpt, FL, US","clouds":[{"cover":"FEW","base":3500},{"cover":"SCT","base":5000},{"cover":"SCT",
-- "base":25000}]}]

data NumberOrString
    = NumberValue Int
    | NumStringValue String
    deriving (Show, Generic)

instance FromJSON NumberOrString where
    parseJSON v = (NumberValue <$> parseJSON v)
              <|> (NumStringValue <$> parseJSON v)

data FloatOrString
    = FloatValue Float
    | FloatStringValue String
    deriving (Show, Generic)

instance FromJSON FloatOrString where
    parseJSON v = (FloatValue <$> parseJSON v)
              <|> (FloatStringValue <$> parseJSON v)

data Clouds = Clouds
    { cover :: String
    , base :: Maybe Int
    } deriving (Show, Generic)

instance FromJSON Clouds

data Weather = Weather
    { metar_id :: Int
    , icaoId :: String
    , receiptTime :: String
    , obsTime :: Integer
    , reportTime :: String
    , temp :: Float
    , dewp :: Float
    , wdir :: NumberOrString
    , wspd :: Int
    , wgst :: Maybe Int
    , visib :: NumberOrString
    , altim :: Float
    , slp :: Float
    , qcField :: Int
    , wxString :: Maybe String
    , presTend :: Maybe String
    , maxT :: Maybe Float
    , minT :: Maybe Float
    , maxT24 :: Maybe Float
    , minT24 :: Maybe Float
    , precip :: Maybe Float
    , pcp3hr :: Maybe Float
    , pcp6hr :: Maybe Float
    , pcp24hr :: Maybe Float
    , snow :: Maybe Float
    , vertVis :: Float
    , metarType :: String
    , rawOb :: String
    , mostRecent :: Int
    , lat :: Float
    , lon :: Float
    , elev :: Int
    , prior :: Int
    , name :: String
    , clouds :: [Clouds]
    } deriving (Show, Generic)

instance FromJSON Weather

-- Convert Unix timestamp (seconds since epoch) to UTCTime
timestampToUTC :: Integer -> UTCTime
timestampToUTC = posixSecondsToUTCTime . fromIntegral

-- -- Format the UTCTime as a readable string
-- formatDate :: UTCTime -> String
-- formatDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

customTimeZone :: TimeZone
customTimeZone = TimeZone (-300) False "EST5EDT"

timestampToCustomTZ :: Integer -> ZonedTime
timestampToCustomTZ timestamp =
    let utcTime = timestampToUTC timestamp
    in utcToZonedTime customTimeZone utcTime

numProcessField :: NumberOrString -> String
numProcessField (NumberValue n) = show n
numProcessField (NumStringValue s) = s

floatProcessField :: FloatOrString -> String
floatProcessField (FloatValue n) = show n
floatProcessField (FloatStringValue s) = s

makeGetRequest :: String -> IO (Either String [Weather])
makeGetRequest url = do
    request <- parseRequest url
    response <- httpLBS request
    -- putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    -- return $ L.unpack (getResponseBody response)
    return $ eitherDecode (getResponseBody response)

outputData :: Weather -> IO ()
outputData weather = do
    printf "Name: %s\n" $ name weather

    -- let date = timestampToUTC $ obsTime weather
    -- let obsTimeStr = formatDate date

    let localTime = timestampToCustomTZ $ obsTime weather

    printf "Observation time: %s\n" $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" localTime
    printf "METAR: %s\n" $ rawOb weather
    printf "Wind Dir: %s\n" $ numProcessField $ wdir weather
    printf "Wind Speed: %d\n" $ wspd weather
    printf "Altimeter: %0.2f\n" $ altim weather
    printf "Visibility: %s\n" $ numProcessField $ visib weather

    case pcp24hr weather of
        Just number -> printf "Precip last 24 hours: %0.2f\n" number
        Nothing -> printf "Precip last 24 hours: None.\n"

    printf "Elevation: %dft ASL\n" $ elev weather

main :: IO ()
main = do
    args <- getArgs

    when (null args) $ do
        putStrLn "Error: No arguments provided"
        exitFailure

    let location = case args of
            [arg1] -> arg1
            _ -> "KTPA"

    let urlStr = "https://aviationweather.gov/api/data/metar?ids=" ++ location ++ "&format=json"

    result <- makeGetRequest urlStr

    case result of
        Right weather -> mapM_ outputData weather -- (take 2 posts)  -- Show first 2 posts
        Left err -> putStrLn err
