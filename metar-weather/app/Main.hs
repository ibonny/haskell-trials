{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Aeson (FromJSON, eitherDecode, parseJSON)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import GHC.Generics
import Network.HTTP.Simple
import System.Environment
import System.Exit
import Text.Printf (printf)
import Data.Maybe (fromMaybe)

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
  parseJSON v =
    (NumberValue <$> parseJSON v)
      <|> (NumStringValue <$> parseJSON v)

data FloatOrString
  = FloatValue Float
  | FloatStringValue String
  deriving (Show, Generic, Eq)

instance FromJSON FloatOrString where
  parseJSON v =
    (FloatValue <$> parseJSON v)
      <|> (FloatStringValue <$> parseJSON v)

data Clouds = Clouds
  { cover :: String,
    base :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON Clouds

data Weather = Weather
  { metar_id :: Int,
    icaoId :: String,
    receiptTime :: String,
    obsTime :: Integer,
    reportTime :: String,
    temp :: Float,
    dewp :: Float,
    wdir :: NumberOrString,
    wspd :: Int,
    wgst :: Maybe Int,
    visib :: NumberOrString,
    altim :: Float,
    slp :: Float,
    qcField :: Int,
    wxString :: Maybe String,
    presTend :: Maybe FloatOrString,
    maxT :: Maybe Float,
    minT :: Maybe Float,
    maxT24 :: Maybe Float,
    minT24 :: Maybe Float,
    precip :: Maybe FloatOrString,
    pcp3hr :: Maybe Float,
    pcp6hr :: Maybe Float,
    pcp24hr :: Maybe Float,
    snow :: Maybe Float,
    vertVis :: Float,
    metarType :: String,
    rawOb :: String,
    mostRecent :: Int,
    lat :: Float,
    lon :: Float,
    elev :: Int,
    prior :: Int,
    name :: String,
    clouds :: [Clouds]
  }
  deriving (Show, Generic)

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

numProcessFieldToNum :: NumberOrString -> Int
numProcessFieldToNum (NumberValue n) = n
numProcessFieldToNum (NumStringValue _) = 0

floatProcessField :: FloatOrString -> String
floatProcessField (FloatValue n) = show n
floatProcessField (FloatStringValue s) = s

floatProcessFieldToFloat :: FloatOrString -> Float
floatProcessFieldToFloat (FloatValue n) = n
floatProcessFieldToFloat (FloatStringValue _) = 0

-- Conversion functions

metersToFeet :: Int -> Float
metersToFeet e = fromIntegral e * 3.28084

knotsToMph :: Int -> Float
knotsToMph windSpeedKt = fromIntegral windSpeedKt * 1.15078

celciusToFahrenheight :: Float -> Float
celciusToFahrenheight cValue = 9/5 * cValue + 32

calculateRelativeHumidity :: Float -> Float -> Float
calculateRelativeHumidity temp_c dewpoint_c =
  -- Magnus formula coefficients
  let a = 17.625
      b = 243.04
      alpha = (a * dewpoint_c) / (b + dewpoint_c)
      beta  = (a * temp_c) / (b + temp_c)
   in 100 * exp (alpha - beta)

-- Get request function
makeGetRequest :: String -> IO (Either String [Weather])
makeGetRequest url = do
  request <- parseRequest url
  response <- httpLBS request
  -- putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
  -- return $ L.unpack (getResponseBody response)
  return $ eitherDecode (getResponseBody response)

getWindDirString :: Int -> String
getWindDirString val
    | val == 0 = "North"
    | val > 0 && val < 45 = "North North-East"
    | val == 45 = "North East"
    | val > 45 && val < 90 = "East North-East"
    | val == 90 = "East"
    | val > 90 && val < 135 = "East South-East"
    | val == 135 = "South East"
    | val > 135 && val < 180 = "South South-East"
    | val == 180 = "South"
    | val > 180 && val < 225 = "South South-West"
    | val == 225 = "South West"
    | val > 225 && val < 275 = "West South-West"
    | val == 270 = "West"
    | val > 270 && val < 315 = "West North-West"
    | val == 315 = "North West"
    | val > 315 && val < 360 = "North North-West"
    | otherwise = printf "%d" val

defaultNumStringValue :: NumberOrString
defaultNumStringValue = NumStringValue "0"

defaultFloatStringValue :: FloatOrString
defaultFloatStringValue = FloatStringValue "0"

outputData :: Weather -> IO ()
outputData weather = do
  printf "Name: %s\n" $ name weather

  -- let date = timestampToUTC $ obsTime weather
  -- let obsTimeStr = formatDate date

  let localTime = timestampToCustomTZ $ obsTime weather

  printf "Observation time: %s\n" $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" localTime
  printf "METAR: %s\n" $ rawOb weather

  if wspd weather /= 0 then do
    printf "Wind Dir: %s (%s)\n" (getWindDirString $ numProcessFieldToNum $ wdir weather) (numProcessField $ wdir weather)
    printf "Wind Speed: %0.2fmph " $ knotsToMph $ wspd weather
  else
    printf "No wind"

  if fromMaybe 0 (wgst weather) /= 0 then
    printf "gusting to %0.2fmph\n" $ knotsToMph $ fromMaybe 0 $ wgst weather
  else
    printf "\n"

  printf "Temperature: %0.2fF\n" $ celciusToFahrenheight $ temp weather
  printf "Dew Point: %0.2fF\n" $ celciusToFahrenheight $ dewp weather

  printf "Relative Humidity: %0.2f%%\n" $ calculateRelativeHumidity (temp weather) (dewp weather)

  if floatProcessFieldToFloat (fromMaybe defaultFloatStringValue (presTend weather)) /= 0 then
    printf "Pressure Tendency: %s mBar\n" $ floatProcessField $ fromMaybe defaultFloatStringValue $ presTend weather
  else
    printf ""

  printf "Altimeter: %0.2f mb\n" $ altim weather

  printf "Sea Level Pressure: %0.2f mb\n" $ slp weather

  printf "Visibility: %s statute miles\n" $ numProcessField $ visib weather

  if fromMaybe defaultFloatStringValue (precip weather) /= defaultFloatStringValue then
    printf "Precipitation: %s in\n" $ floatProcessField $ fromMaybe defaultFloatStringValue $ precip weather
  else
    printf ""

  case pcp24hr weather of
    Just number -> printf "Precip last 24 hours: %0.2f\n" number
    Nothing -> printf ""

  printf "Elevation: %0.2fft MSL\n" $ metersToFeet $ elev weather

loopData :: [Weather] -> IO ()
loopData weather =
    if null weather then
        printf "Airport cannot be found.\n"
    else
        mapM_ outputData weather

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
    Right weather -> loopData weather
    Left err -> putStrLn err
