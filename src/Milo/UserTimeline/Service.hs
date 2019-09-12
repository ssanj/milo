{-# LANGUAGE OverloadedStrings #-}

module Milo.UserTimeline.Service (getUserTimeline) where

import Milo.Oauth2
import Milo.Config
import Milo.Model
import Milo.Oauth2.Model
import Milo.UserTimeline.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import qualified Network.HTTP.Types          as Client
import Data.Aeson (fromJSON)

getUserTimeline :: Env -> Client.Manager -> TwitterUser -> IO (Either String [UserTimeline])
getUserTimeline env manager tuser = performAction env manager bearerRequestProvider $ userRequestProvider tuser

--- access token retrieval 
tokenUrl :: String
tokenUrl = "https://api.twitter.com/oauth2/token"

bearerRequestProvider :: OAuth2ClientToken -> RequestProvider IO BearerToken
bearerRequestProvider oauth2ClientToken = 
  RequestProvider $ (addBasicAuth oauth2ClientToken . addTokenFormParams) <$> Client.parseRequest tokenUrl

--- get mentions 
userTimelineUrl :: String
userTimelineUrl = "https://api.twitter.com/1.1/statuses/user_timeline.json"

userRequestProvider :: TwitterUser -> BearerToken -> RequestProvider IO [UserTimeline]
userRequestProvider tuser bearer =
  RequestProvider $ (addBearerTokenAuth bearer . addQueryParams tuser) <$> Client.parseRequest userTimelineUrl

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "2")

twitterUserParam :: TwitterUser -> (C8.ByteString, Maybe C8.ByteString)
twitterUserParam (TwitterUser tuser)= ("screen_name", Just tuser)

addBasicAuth :: OAuth2ClientToken -> Client.Request -> Client.Request
addBasicAuth (OAuth2ClientToken (ClientKey key) (ClientSecret secret)) = Client.applyBasicAuth key secret

addBearerTokenAuth :: BearerToken -> Client.Request -> Client.Request
addBearerTokenAuth (BearerToken _ accessToken) req = 
  let authHeader = (Client.hAuthorization, "Bearer " <> C8.pack accessToken)
      existingHeaders = Client.requestHeaders req
      allHeaders = authHeader : existingHeaders
  in req { Client.requestHeaders = allHeaders }

addTokenFormParams :: Client.Request -> Client.Request
addTokenFormParams = Client.urlEncodedBody [("grant_type", "client_credentials")]

addQueryParams :: TwitterUser -> Client.Request -> Client.Request
addQueryParams tuser = Client.setQueryString [countParam, twitterUserParam tuser]