{-# LANGUAGE DeriveGeneric #-}

module Milo.MentionsTimeline.Model where

import GHC.Generics
import Data.Aeson (FromJSON)

data MentionsTimelineUser = MentionsTimelineUser { name :: !String, screen_name :: !String } deriving (Generic, Show)

data MentionsTimeline = 
  MentionsTimeline { 
    created_at :: !String, 
    user :: MentionsTimelineUser,
    text :: !String, 
    lang :: !String
  } deriving (Generic, Show)  

instance FromJSON MentionsTimelineUser where
instance FromJSON MentionsTimeline where
