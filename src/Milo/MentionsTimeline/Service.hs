{-# LANGUAGE OverloadedStrings #-}

module Milo.MentionsTimeline.Service (getMentionsTimeline) where

import Milo.Oauth1.Controller
import Milo.Model
import Milo.Config.Model (Env)
import Milo.Request

import qualified Network.HTTP.Client         as Client

getMentionsTimeline :: Env -> Client.Manager -> IO (Either String [Tweet])
getMentionsTimeline env manager = performAction env manager $ defaultRequestProvider mentionsTimelineUrl

mentionsTimelineUrl :: String
mentionsTimelineUrl = "https://api.twitter.com/1.1/statuses/mentions_timeline.json"
