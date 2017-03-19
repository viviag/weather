-- Datatypes module group
{-# LANGUAGE DeriveGeneric #-}

module Condition where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Condition = Condition
  { id :: Int
  , main :: String
  , description :: String
  , icon :: String
  } deriving (Generic, Show)

instance FromJSON Condition
