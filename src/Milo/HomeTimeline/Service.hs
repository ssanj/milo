{-# LANGUAGE OverloadedStrings #-}

module Milo.HomeTimeline.Service (getHomeTimeline) where

import Milo.Oauth1
import Milo.Config
import Milo.Model
import Milo.HomeTimeline.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import Data.Aeson (Result(..), fromJSON)

getHomeTimeline :: Env -> Client.Manager -> IO (Either String [HomeTimeline])
getHomeTimeline env manager = do
  req               <- addQueryParams <$> Client.parseRequest homeTimelineUrl
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
      decodeJson :: Result [HomeTimeline] -> Either String [HomeTimeline]
      decodeJson (Success value) = Right value
      decodeJson (Error error) = Left error

homeTimelineUrl :: String
homeTimelineUrl = "https://api.twitter.com/1.1/statuses/home_timeline.json"

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "100")

addQueryParams :: Client.Request -> Client.Request
addQueryParams = Client.setQueryString [countParam]