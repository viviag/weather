{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Condition
import qualified Sys

import Data.Aeson
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text (split, pack, unpack, empty)
import GHC.Generics (Generic)

import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Builder as Opt
import qualified Servant.API as Net
import qualified Servant.Client as Net
import System.Posix.Env (getEnv)

type Press = Float
type Temperature = Float

data Coord = Coord
  { lon :: Float
  , lat :: Float
  } deriving (Generic, Show)

data Dat = Dat
  { temp :: Temperature
  , pressure :: Press
  , humidity :: Int
  , temp_min :: Temperature
  , temp_max :: Temperature
  , sea_level :: Maybe Press
  , grnd_level :: Maybe Press
  } deriving Show

data Wind = Wind
  { speed :: Float
  , deg :: Float
  } deriving (Generic, Show)

data Clouds = Clouds
  { all :: Int
  } deriving (Generic, Show)

data Rain = Rain
  { r3h :: Int
  } deriving Show

data Snow = Snow
  { s3h :: Int
  } deriving Show

data Weather = Weather
  { coord :: Coord
  , weather :: [Condition.Condition]
  , base :: String
  , main :: Dat
  , wind :: Wind
  , clouds :: Maybe Clouds
  , rain :: Maybe Rain
  , snow :: Maybe Snow
  , dt :: Int
  , sys :: Sys.Sys
  , id :: Int
  , name :: String
  , cod :: Int
  } deriving Show

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

-- Type of request to api.openweathermap.org
type Api        = "weather" Net.:> Net.QueryParam "q" String
                            Net.:> Net.QueryParam "APPID" String
                            Net.:> Net.Get '[Net.JSON] Weather
         Net.:<|> "weather" Net.:> Net.QueryParam "lat" String 
                            Net.:> Net.QueryParam "lon" String 
                            Net.:> Net.QueryParam "APPID" String
                            Net.:> Net.Get '[Net.JSON] Weather

-- connection to api (http://haskell-servant.readthedocs.io/en/stable/tutorial/Client.html)
weatherCity :: Maybe String   -- City name
            -> Maybe String   -- API key
            -> Net.ClientM Weather
weatherCoord :: Maybe String    -- Latitude (not parsed to int yet)
             -> Maybe String    -- Longitude (also not)
             -> Maybe String    -- API key
             -> Net.ClientM Weather

api :: Proxy Api  -- Needed to bind api and response handlers
api = Proxy

weatherCity Net.:<|> weatherCoord = Net.client api

-- Constructing queries
queryCity :: Maybe String -- Api key
          -> Maybe String -- City name
          -> Net.ClientM Weather
queryCity key name = do
  weather <- weatherCity name key
  return weather

queryCoord :: Maybe String -- Api key
           -> Maybe String -- Latitude
           -> Maybe String -- Longitude
           -> Net.ClientM Weather
queryCoord key lat lon = do
  weather <- weatherCoord lat lon key
  return weather

-- Here to be switching skin for queries, will parse args.
query :: Maybe String -- API key
      -> [String] -- Parameters
      -> Net.ClientM Weather
query key (x:xs)
  | xs == [] = queryCity key (Just x)
  | otherwise = queryCoord key (Just x) (Just (head xs))

-- Printer
weatherPrint :: Weather -> IO ()
weatherPrint x = do
  putStrLn $ "Coordinates: " ++ show (coord x)
  putStrLn $ "Conditions: " ++ show (weather x)
  putStrLn $ "Weather data: " ++ show (main x)
  case rain x of
    Nothing -> do
      return ()
    Just (r3h) -> do
      print r3h
  case clouds x of
    Nothing -> do
      return ()
    Just (all) -> do
      print all
  case snow x of
    Nothing -> do
      return ()
    Just (s3h) -> do
      print s3h
  putStrLn "Country info (sunrise and sunset):"
  putStrLn (Sys.country (sys x))
  putStrLn "In Unix time format"
  case Sys.sunrise (sys x) of
    Nothing -> do
      return ()
    Just (sunrise) -> do
      print sunrise
  case Sys.sunset (sys x) of
    Nothing -> do
      return ()
    Just (sunset) -> do
      print sunset
  putStrLn "Date:"
  print $ dt x
  putStrLn $ "In location: " ++ (name x)

-- main
weatherCall :: IO ()
weatherCall = do
  manager <- newManager defaultManagerSettings
  key <- getEnv "OPENWEATHER"
  args <- Opt.execParser (
            Opt.info 
              (Opt.some (Opt.argument Opt.str (Opt.metavar "Location"))) 
              (Opt.fullDesc
               <> Opt.progDesc "Calls OpenWeatherMap API"
               <> Opt.header "Parser")
          )
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
    Right (weather) -> do
      weatherPrint weather
