{-# LANGUAGE OverloadedStrings #-}

module Milo.UserTimeline.Service (getUserTimeline) where

import Milo.Oauth2.Controller
import Milo.Config
import Milo.Model
import Milo.Request
import Milo.Oauth2.Model

import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import qualified Network.HTTP.Types          as Client

getUserTimeline :: Env -> Client.Manager -> TwitterHandle -> IO (Either String [Tweet])
getUserTimeline env manager = performAction env manager . userRequestProvider

--- get mentions 
userTimelineUrl :: String
userTimelineUrl = "https://api.twitter.com/1.1/statuses/user_timeline.json"

userRequestProvider :: TwitterHandle -> RequestProvider IO [Tweet]
userRequestProvider tuser  =
  RequestProvider $ addQueryParams tuser <$> Client.parseRequest userTimelineUrl

twitterHandleParam :: TwitterHandle -> (C8.ByteString, Maybe C8.ByteString)
twitterHandleParam (TwitterHandle tuser)= ("screen_name", Just tuser)

addQueryParams :: TwitterHandle -> Client.Request -> Client.Request
addQueryParams tuser = Client.setQueryString [countParam, twitterHandleParam tuser, extendedTweetParam]