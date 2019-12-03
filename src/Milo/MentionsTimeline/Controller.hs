{-# LANGUAGE OverloadedStrings #-}

module Milo.MentionsTimeline.Controller (mentionsTimelineAction) where

import Milo.Model
import Milo.MentionsTimeline.Service

import Data.Bifunctor (bimap)

import Milo.Config.Model (Env)

import qualified Network.HTTP.Client as Client
import qualified Data.Text as T

endpoint :: T.Text
endpoint = "Mentions Timeline"

mentionsTimelineAction :: Env -> Client.Manager -> TweetResultIO Tweet
mentionsTimelineAction env manager = convertResults <$> getMentionsTimeline env manager
  where 
        heading = Heading MentionHeading endpoint
        convertResults :: Either String [Tweet] -> TweetResult Tweet
        convertResults = bimap (\e -> TweetRetrievalError heading (TwitterEndpoint endpoint) (twitterError e)) 
                               (TweetOutput heading)
