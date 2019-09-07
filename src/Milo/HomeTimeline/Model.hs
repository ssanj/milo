{-# LANGUAGE DeriveGeneric #-}

module Milo.HomeTimeline.Model where

import GHC.Generics
import Data.Aeson (FromJSON)

data HomeTimeLineUser = HomeTimeLineUser { name :: !String, screen_name :: !String } deriving (Generic, Show)

data HomeTimeline = 
  HomeTimeline { 
    created_at :: !String, 
    user :: HomeTimeLineUser,
    text :: !String, 
    lang :: !String
  } deriving (Generic, Show)  

instance FromJSON HomeTimeLineUser where
instance FromJSON HomeTimeline where
