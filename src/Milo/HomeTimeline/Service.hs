{-# LANGUAGE OverloadedStrings #-}

module Milo.HomeTimeline.Service (getHomeTimeline) where

import Milo.Oauth1
import Milo.Config
import Milo.Model
import Milo.HomeTimeline.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import Data.Aeson (fromJSON)

getHomeTimeline :: Env -> Client.Manager -> IO (Either String [HomeTimeline])
getHomeTimeline env manager = performAction env manager requestProvider

requestProvider :: RequestProvider IO [HomeTimeline]
requestProvider = RequestProvider $ addQueryParams <$> Client.parseRequest homeTimelineUrl

homeTimelineUrl :: String
homeTimelineUrl = "https://api.twitter.com/1.1/statuses/home_timeline.json"

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "100")

addQueryParams :: Client.Request -> Client.Request
addQueryParams = Client.setQueryString [countParam]