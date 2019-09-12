{-# LANGUAGE DeriveGeneric #-}

module Milo.UserTimeline.Model where

import GHC.Generics
import Data.Aeson (FromJSON)

-- TODO: Timeline user is common. Reuse with possible phantom types
data UserTimelineUser = UserTimelineUser { name :: !String, screen_name :: !String } deriving (Generic, Show)

data UserTimeline = 
  UserTimeline { 
    created_at :: !String, 
    user :: UserTimelineUser,
    text :: !String, 
    lang :: !String
  } deriving (Generic, Show)  

instance FromJSON UserTimelineUser where
instance FromJSON UserTimeline where
