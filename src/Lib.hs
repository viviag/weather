{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import qualified Data.Proxy as Proxy
import Data.Semigroup ((<>))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import GHC.Generics (Generic)

import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Options.Applicative as Opt
import Options.Applicative.Builder ()
import qualified Servant.API as Net
import qualified Servant.Client as Net
import System.Posix.Env (getEnv)

-- |Reason to take out:
-- name collisions and wishing to use generic FromJSON instances
import qualified Condition
import qualified Sys

type Press = Float
type Temperature = Float

-- |Here and then are records corresponding to
-- https://openweathermap.org/current
data Coord = Coord
  { lon :: Float -- ^ Longitude of place
  , lat :: Float -- ^ Latitude of place
  } deriving (Generic, Show)

-- Optional fields - reason not to derive generics
data Dat = Dat
  { temp :: Temperature -- ^ Temperarure, Kelvin
  , pressure :: Press -- ^ Atmospheric pressure, hPa
  , humidity :: Int -- ^ Percentile of humidity
  , temp_min :: Temperature -- ^ Miminal temperature for broad locations
  , temp_max :: Temperature -- ^ Maximal temperature for broad locations
  , sea_level :: Maybe Press -- ^ Pressure on a sea level, hPa
  , grnd_level :: Maybe Press -- ^ Pressure on a ground level, hPa
  } deriving Show

data Wind = Wind
  { speed :: Float -- ^ Wind speed, meter/sec
  , deg :: Float -- ^ Wind direction, degrees
  } deriving (Generic, Show)

data Clouds = Clouds
  { all :: Int -- ^ Cloudness, %
  } deriving (Generic, Show)

-- Name of field in json starts from integer, need to specify FromJSON instance
data Rain = Rain
  { r3h :: Int -- ^ Rain volume for last 3 hours
  } deriving Show

data Snow = Snow
  { s3h :: Int -- ^ Snow volume for last 3 hours
  } deriving Show

-- Optionals
data Weather = Weather
  { coord :: Coord -- ^ Location
  , weather :: [Condition.Condition] -- ^ array of weather conditions
  , base :: String -- ^ Internal
  , main :: Dat -- ^ Weather data
  , wind :: Wind -- ^ Wind data
  , clouds :: Maybe Clouds -- ^ Clouds data
  , rain :: Maybe Rain -- ^ Rain data
  , snow :: Maybe Snow -- ^ Snow data
  , dt :: POSIXTime -- ^ Time of calculation, unix, UTC
  , sys :: Sys.Sys -- ^ General info about location
  , id :: Int -- ^ weather id
  , name :: String -- ^ Location Name
  , cod :: Int -- ^ code, which server returns on a request
  } deriving Show

-- .:! - optionally exists
instance FromJSON Weather where
  parseJSON = withObject "init" $ \v -> Weather
              <$> v .: "coord"
              <*> v .: "weather"
              <*> v .: "base"
              <*> v .: "main"
              <*> v .: "wind"
              <*> v .:! "clouds"
              <*> v .:! "rain"
              <*> v .:! "snow"
              <*> v .: "dt"
              <*> v .: "sys"
              <*> v .: "id"
              <*> v .: "name"
              <*> v .: "cod"
instance FromJSON Coord
instance FromJSON Dat where
  parseJSON = withObject "main" $ \v -> Dat
                          <$> v .: "temp"
                          <*> v .: "pressure"
                          <*> v .: "humidity"
                          <*> v .: "temp_min"
                          <*> v .: "temp_max"
                          <*> v .:! "sea_level"
                          <*> v .:! "grnd_level"
instance FromJSON Wind
instance FromJSON Clouds
instance FromJSON Rain where
  parseJSON = withObject "rain" $ \v -> Rain
                          <$> v .: "3h"
instance FromJSON Snow where
  parseJSON = withObject "snow" $ \v -> Snow
                          <$> v .: "3h"

-- |Type of request to api.openweathermap.org
-- :> - equivalent to / or concatenation of one level parameters.
type Api        = "weather" Net.:> Net.QueryParam "q" String
                            Net.:> Net.QueryParam "APPID" String
                            Net.:> Net.Get '[Net.JSON] Weather
        -- Alternative request
         Net.:<|> "weather" Net.:> Net.QueryParam "lat" String 
                            Net.:> Net.QueryParam "lon" String 
                            Net.:> Net.QueryParam "APPID" String
                            Net.:> Net.Get '[Net.JSON] Weather

-- |Connection to api
-- (http://haskell-servant.readthedocs.io/en/stable/tutorial/Client.html)
-- Here and then Maybe in request parameters - params interpreted as options
weatherCity :: Maybe String   -- ^ City name
            -> Maybe String   -- ^ API key
            -> Net.ClientM Weather
weatherCoord :: Maybe String    -- ^ Latitude (not parsed to int yet)
             -> Maybe String    -- ^ Longitude (also not)
             -> Maybe String    -- ^ API key
             -> Net.ClientM Weather

api :: Proxy.Proxy Api  -- Needed to bind api and response handlers
api = Proxy.Proxy

-- Constucting correspondance between API and API calls
weatherCity Net.:<|> weatherCoord = Net.client api

-- Constructing parametric queries
queryCity :: Maybe String -- ^ Api key
          -> Maybe String -- ^ City name
          -> Net.ClientM Weather
queryCity key city = do
  weath <- weatherCity city key
  return weath

queryCoord :: Maybe String -- ^ Api key
           -> Maybe String -- ^ Latitude
           -> Maybe String -- ^ Longitude
           -> Net.ClientM Weather
queryCoord key latid long = do
  weath <- weatherCoord latid long key
  return weath

-- Switching skin for queries.
query :: Maybe String -- ^ API key
      -> [String] -- ^ Parameters
      -> Net.ClientM Weather
query key (x:xs)
  | xs == [] = queryCity key (Just x)
  | otherwise = queryCoord key (Just x) (Just (head xs))
-- |never reached due to error from parser.
-- needed to avoid warning.
query key [] = queryCity key (Just "Moscow")

-- |Printer
weatherPrint :: Weather -- ^ Weather record to print
             -> IO ()
weatherPrint x = do
  putStrLn $ "Coordinates: " ++ show (coord x)
  putStrLn $ "Conditions: " ++ show (weather x)
  putStrLn $ "Weather data: " ++ show (main x)
  case rain x of
    Nothing -> do
      return ()
    Just (item) -> do
      print item
  case clouds x of
    Nothing -> do
      return ()
    Just (item) -> do
      print item
  case snow x of
    Nothing -> do
      return ()
    Just (item) -> do
      print item
  putStrLn "Country info (sunrise and sunset):"
  putStrLn (Sys.country (sys x))
  case Sys.sunrise (sys x) of
    Nothing -> do
      return ()
    Just (sunrise) -> do
      print $ posixSecondsToUTCTime sunrise
  case Sys.sunset (sys x) of
    Nothing -> do
      return ()
    Just (sunset) -> do
      print $ posixSecondsToUTCTime sunset
  putStrLn "Date:"
  print $ posixSecondsToUTCTime $ dt x
  putStrLn $ "In location: " ++ (name x)

-- Main - read command line, call API, print
weatherCall :: IO ()
weatherCall = do
  -- Connection manager, used by servant
  manager <- newManager defaultManagerSettings
  -- unix library getEnv, looks up and don't fall.
  key <- getEnv "OPENWEATHER"
  -- |argument parser with direct execution from optparse.
  -- " " - end mark of Opt.str
  args <- Opt.execParser (
            Opt.info 
              (Opt.some (Opt.argument Opt.str (Opt.metavar "Location"))) 
              (Opt.fullDesc
               <> Opt.progDesc "Calls OpenWeatherMap API"
               <> Opt.header "Parser")
          )
  -- Running query via manager, result is instance of Either
  result <- Net.runClientM
              (query key args) (
                Net.ClientEnv manager (
                Net.BaseUrl
                  Net.Http
                  "api.openweathermap.org"
                  80 
                  "/data/2.5"
                )
              )
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (weath) -> do
      weatherPrint weath
