{-# LANGUAGE OverloadedStrings #-}

module Milo.Oauth2.Controller where

import qualified Network.OAuth               as OA
import qualified Network.OAuth.Types.Params  as OA
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as LS
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import qualified Network.HTTP.Types          as Client
import qualified Network.HTTP.Types.Header   as Client
import Data.CaseInsensitive (mk)
import Milo.Config
import Milo.Model
import Milo.Oauth2.Model
import Milo.Request (makeRequest)
import Data.Aeson (FromJSON, Value, eitherDecodeStrict', Result(..), fromJSON)
import Data.Aeson.Types (parseEither)

performAction :: FromJSON a => Env -> Client.Manager -> RequestProvider IO a -> IO (Either String a)
performAction env manager reqProvider = do
  let 
      clientKey         = _clientKey . _env $ env
      clientSecret      = _clientSecret . _env$  env
      clientToken       = OAuth2ClientToken clientKey clientSecret
      config            = _config env
  tokenResponseE <- getRequest (bearerRequestProvider clientToken) >>= makeRequest config manager
  case tokenResponseE of
    Right bearer -> (addBearerTokenAuth bearer <$> getRequest reqProvider) >>= makeRequest config manager
    Left error -> pure . Left $ error

tap :: Show a => IO a -> (a -> IO ()) -> IO a
tap ioa f = ioa >>= f >> ioa

--- access token retrieval 
tokenUrl :: String
tokenUrl = "https://api.twitter.com/oauth2/token"

bearerRequestProvider :: OAuth2ClientToken -> RequestProvider IO BearerToken
bearerRequestProvider oauth2ClientToken = 
  RequestProvider $ addBasicAuth oauth2ClientToken . addTokenFormParams <$> Client.parseRequest tokenUrl

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
