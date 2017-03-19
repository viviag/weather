-- Datatypes module group
{-# LANGUAGE OverloadedStrings #-}

module Sys where

import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)

data Sys = Sys
  { typ :: Maybe Int -- ^ Internal parameter
  , id :: Maybe Int -- ^ Internal
  , message :: Float -- ^ Internal
  , country :: String -- ^ Country code
  , sunrise :: Maybe POSIXTime -- ^ Sunrise time, unix, UTC
  , sunset :: Maybe POSIXTime -- ^ Sunset time, unix, UTC
  } deriving Show

instance FromJSON Sys where
  parseJSON = withObject "sys" $ \v -> Sys
                          <$> v .:! "type"
                          <*> v .:! "id"
                          <*> v .: "message"
                          <*> v .: "country"
                          <*> v .:! "sunrise"
                          <*> v .:! "sunset"
