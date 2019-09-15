{-# LANGUAGE OverloadedStrings #-}

module Milo.MentionsTimeline.Service (getMentionsTimeline) where

import Milo.Oauth1
import Milo.Config
import Milo.Model
-- import Milo.MentionsTimeline.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import Data.Aeson (fromJSON)

getMentionsTimeline :: Env -> Client.Manager -> IO (Either String [Tweet])
getMentionsTimeline env manager = performAction env manager requestProvider

requestProvider :: RequestProvider IO [Tweet]
requestProvider = RequestProvider $ addQueryParams <$> Client.parseRequest mentionsTimelineUrl

mentionsTimelineUrl :: String
mentionsTimelineUrl = "https://api.twitter.com/1.1/statuses/mentions_timeline.json"

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "2")

extendedTweetParam :: (C8.ByteString, Maybe C8.ByteString)
extendedTweetParam = ("tweet_mode", Just "extended")

addQueryParams :: Client.Request -> Client.Request
addQueryParams = Client.setQueryString [countParam, extendedTweetParam]