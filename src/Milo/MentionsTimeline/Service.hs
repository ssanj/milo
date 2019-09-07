{-# LANGUAGE OverloadedStrings #-}

module Milo.MentionsTimeline.Service (getMentionsTimeline) where

import Milo.Oauth1
import Milo.Config
import Milo.Model
import Milo.MentionsTimeline.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import Data.Aeson (Result(..), fromJSON)

getMentionsTimeline :: Env -> Client.Manager -> IO (Either String [MentionsTimeline])
getMentionsTimeline env manager = do
  req               <- addQueryParams <$> Client.parseRequest mentionsTimelineUrl
  let 
      clientKey         = unClientKey . _clientKey $ env
      clientSecret      = unClientSecret . _clientSecret $ env
      accessToken       = unAccessToken . _accessToken $ env
      accessTokenSecret = unAccessTokenSecret . _accessTokenSecret $ env
      clientToken       = clientOauthToken (OAuthToken clientKey) (OAuthTokenSecret clientSecret)
      clientCred        = oauthClientCred clientToken
      permToken         = permanentOauthToken (OAuthToken accessToken) (OAuthTokenSecret accessTokenSecret)
      permCred          = oauthPermanentCred permToken clientCred
  signedReq <- signRequest permCred oserver req
  response  <- makeRequest manager signedReq
  pure $ case response of
    Left error -> Left $ "Couldn't decode response due to " <> error
    Right json -> decodeJson $ fromJSON json where
      decodeJson :: Result [MentionsTimeline] -> Either String [MentionsTimeline]
      decodeJson (Success value) = Right value
      decodeJson (Error error) = Left error

mentionsTimelineUrl :: String
mentionsTimelineUrl = "https://api.twitter.com/1.1/statuses/mentions_timeline.json"

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "20")

addQueryParams :: Client.Request -> Client.Request
addQueryParams = Client.setQueryString [countParam]