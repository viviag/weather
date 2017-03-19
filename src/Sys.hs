-- Datatypes module group
{-# LANGUAGE OverloadedStrings #-}

module Sys where

import Data.Aeson
import Data.Time.Clock (UTCTime)

data Sys = Sys
  { typ :: Maybe Int
  , id :: Maybe Int
  , message :: Float
  , country :: String
  , sunrise :: Maybe Int
  , sunset :: Maybe Int
  } deriving Show

instance FromJSON Sys where
  parseJSON = withObject "sys" $ \v -> Sys
                          <$> v .:! "type"
                          <*> v .:! "id"
                          <*> v .: "message"
                          <*> v .: "country"
                          <*> v .:! "sunrise"
                          <*> v .:! "sunset"
