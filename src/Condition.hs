-- Datatypes module group
{-# LANGUAGE DeriveGeneric #-}

module Condition where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Condition = Condition
  { id :: Int -- ^ Weather condition id
  , main :: String -- ^ Group of weather parameters
  , description :: String -- ^ Condition within the group
  , icon :: String -- ^ Weather icon id
  } deriving (Generic, Show)

instance FromJSON Condition
