{-# LANGUAGE OverloadedStrings #-}

module Milo.HomeTimeline.Service (getHomeTimeline) where

import Milo.Oauth1
import Milo.Config
import Milo.Model
-- import Milo.HomeTimeline.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import Data.Aeson (fromJSON)

getHomeTimeline :: Env -> Client.Manager -> IO (Either String [Tweet])
getHomeTimeline env manager = performAction env manager requestProvider

requestProvider :: RequestProvider IO [Tweet]
requestProvider = RequestProvider $ addQueryParams <$> Client.parseRequest homeTimelineUrl

homeTimelineUrl :: String
homeTimelineUrl = "https://api.twitter.com/1.1/statuses/home_timeline.json"

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "2")

extendedTweetParam :: (C8.ByteString, Maybe C8.ByteString)
extendedTweetParam = ("tweet_mode", Just "extended")

addQueryParams :: Client.Request -> Client.Request
addQueryParams = Client.setQueryString [countParam, extendedTweetParam]