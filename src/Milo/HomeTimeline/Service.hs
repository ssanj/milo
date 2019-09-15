{-# LANGUAGE OverloadedStrings #-}

module Milo.HomeTimeline.Service (getHomeTimeline) where

import Milo.Oauth1
import Milo.Config
import Milo.Model
import Milo.Request
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client

getHomeTimeline :: Env -> Client.Manager -> IO (Either String [Tweet])
getHomeTimeline env manager = performAction env manager $ defaultRequestProvider homeTimelineUrl

homeTimelineUrl :: String
homeTimelineUrl = "https://api.twitter.com/1.1/statuses/home_timeline.json"
