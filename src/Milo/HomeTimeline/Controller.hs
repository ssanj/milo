{-# LANGUAGE OverloadedStrings #-}

module Milo.HomeTimeline.Controller (homeTimelineAction) where

import Milo.Model
import Milo.HomeTimeline.Service

import Data.Bifunctor (bimap)

import Milo.Config.Model (Env)

import qualified Network.HTTP.Client as Client
import qualified Data.Text as T

endpoint :: T.Text
endpoint = "Home Timeline"

homeTimelineAction :: Env -> Client.Manager -> TweetResultIO Tweet
homeTimelineAction env manager = convertResults <$> getHomeTimeline env manager
  where 
        heading = Heading HomeTimelineHeading endpoint
        convertResults :: Either String [Tweet] -> TweetResult Tweet
        convertResults = bimap (\e -> TweetRetrievalError heading (TwitterEndpoint endpoint) (twitterError e)) 
                           (TweetOutput heading)
