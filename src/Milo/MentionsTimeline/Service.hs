{-# LANGUAGE OverloadedStrings #-}

module Milo.MentionsTimeline.Service (getMentionsTimeline) where

import Milo.Oauth1
import Milo.Config
import Milo.Model
import Milo.MentionsTimeline.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import Data.Aeson (fromJSON)

getMentionsTimeline :: Env -> Client.Manager -> IO (Either String [MentionsTimeline])
getMentionsTimeline env manager = performAction env manager requestProvider

requestProvider :: RequestProvider IO [MentionsTimeline]
requestProvider = RequestProvider $ addQueryParams <$> Client.parseRequest mentionsTimelineUrl

mentionsTimelineUrl :: String
mentionsTimelineUrl = "https://api.twitter.com/1.1/statuses/mentions_timeline.json"

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "2")

addQueryParams :: Client.Request -> Client.Request
addQueryParams = Client.setQueryString [countParam]