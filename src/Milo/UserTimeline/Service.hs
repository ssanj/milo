{-# LANGUAGE OverloadedStrings #-}

module Milo.UserTimeline.Service (getUserTimeline) where

import Milo.Oauth2.Controller
import Milo.Config
import Milo.Model
import qualified Milo.Request as R
import Milo.Oauth2.Model

import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import qualified Network.HTTP.Types          as Client

getUserTimeline :: Env -> Client.Manager -> MentionRequest -> IO (Either String [Tweet])
getUserTimeline env manager = performAction env manager . userRequestProvider

--- get mentions 
userTimelineUrl :: String
userTimelineUrl = "https://api.twitter.com/1.1/statuses/user_timeline.json"

userRequestProvider :: MentionRequest -> RequestProvider IO [Tweet]
userRequestProvider mentionRequest =
  RequestProvider $ addQueryParams mentionRequest <$> Client.parseRequest userTimelineUrl

twitterHandleParam :: TwitterHandle -> (C8.ByteString, Maybe C8.ByteString)
twitterHandleParam (TwitterHandle tuser)= ("screen_name", Just tuser)

addQueryParams :: MentionRequest -> Client.Request -> Client.Request
addQueryParams (MentionRequest tuser count) = Client.setQueryString [numTweets count, twitterHandleParam tuser, R.extendedTweetParam]

numTweets :: TweetCount -> (C8.ByteString, Maybe C8.ByteString)
numTweets (TweetCount count) = ("count", Just . C8.pack $ show count)
