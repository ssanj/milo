{-# LANGUAGE OverloadedStrings #-}

module Milo.UserTimeline.Controller (userTimelineAction) where

import Milo.Model
import Milo.UserTimeline.Service

import Data.Bifunctor (bimap)

import Milo.Config.Model (Env)

import qualified Network.HTTP.Client         as Client
import qualified Data.Text                   as T

endpoint :: T.Text
endpoint = "User Timeline"

userTimelineAction :: Env -> Client.Manager -> MentionRequest -> TweetResultIO Tweet
userTimelineAction env manager mentionRequest = convertResults <$> getUserTimeline env manager mentionRequest
  where 
        heading = Heading UserTimelineHeading $ twitterHandler mentionRequest
        convertResults :: Either String [Tweet] -> TweetResult Tweet
        convertResults = bimap (TweetRetrievalError heading (TwitterEndpoint endpoint) . twitterError) 
                               (TweetOutput heading)
  
twitterHandler :: MentionRequest -> T.Text
twitterHandler (MentionRequest (RealName realName) (TwitterHandle twuser) _) = realName <> " (@" <> twuser <> ")"