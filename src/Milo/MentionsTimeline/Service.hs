{-# LANGUAGE OverloadedStrings #-}

module Milo.MentionsTimeline.Service (getMentionsTimeline) where

import Milo.Oauth2
import Milo.Config
import Milo.Model
import Milo.Oauth2.Model
import Milo.MentionsTimeline.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import qualified Network.HTTP.Types          as Client
import Data.Aeson (fromJSON)

getMentionsTimeline :: Env -> Client.Manager -> IO (Either String [MentionsTimeline])
getMentionsTimeline env manager = performAction env manager bearerRequestProvider mentionsRequestProvider

--- access token retrieval 
tokenUrl :: String
tokenUrl = "https://api.twitter.com/oauth2/token"

bearerRequestProvider :: OAuth2ClientToken -> RequestProvider IO BearerToken
bearerRequestProvider oauth2ClientToken = 
  RequestProvider $ (addBasicAuth oauth2ClientToken . addFormParams) <$> Client.parseRequest tokenUrl

--- get mentions 
mentionsTimelineUrl :: String
mentionsTimelineUrl = "https://api.twitter.com/1.1/statuses/mentions_timeline.json"

mentionsRequestProvider :: BearerToken -> RequestProvider IO [MentionsTimeline]
mentionsRequestProvider bearer =
  RequestProvider $ (addBearerTokenAuth bearer . addQueryParams) <$> Client.parseRequest mentionsTimelineUrl

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "2")

addBasicAuth :: OAuth2ClientToken -> Client.Request -> Client.Request
addBasicAuth (OAuth2ClientToken (ClientKey key) (ClientSecret secret)) = Client.applyBasicAuth key secret

addBearerTokenAuth :: BearerToken -> Client.Request -> Client.Request
addBearerTokenAuth (BearerToken _ accessToken) req = 
  let authHeader = (Client.hAuthorization, "Bearer: " <> C8.pack accessToken)
      existingHeaders = Client.requestHeaders req
      allHeaders = authHeader : existingHeaders
  in req { Client.requestHeaders = allHeaders }

addFormParams :: Client.Request -> Client.Request
addFormParams = Client.urlEncodedBody [("grant_type", "client_credentials")]

addQueryParams :: Client.Request -> Client.Request
addQueryParams = Client.setQueryString [countParam]