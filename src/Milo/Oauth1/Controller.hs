{-# LANGUAGE OverloadedStrings #-}

module Milo.Oauth1.Controller where

import qualified Network.OAuth               as OA
import qualified Network.OAuth.Types.Params  as OA
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as LS
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import Data.Maybe (listToMaybe)
import Network.HTTP.Types.Header (hAuthorization)
import System.Environment (getEnv)
import Data.CaseInsensitive (mk)
import Milo.Config
import Milo.Model
import Milo.Request (makeRequest)
import Milo.Oauth1.Model
import Data.List (intercalate)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (parseEither)

clientOauthToken :: OAuthToken -> OAuthTokenSecret -> OA.Token OA.Client
clientOauthToken (OAuthToken otoken) (OAuthTokenSecret osecret) = OA.Token otoken osecret

permanentOauthToken :: OAuthToken -> OAuthTokenSecret -> OA.Token OA.Permanent
permanentOauthToken (OAuthToken otoken) (OAuthTokenSecret osecret) = OA.Token otoken osecret

oauthClientCred :: OA.Token OA.Client -> OA.Cred OA.Client
oauthClientCred = OA.clientCred 

oauthPermanentCred :: OA.Token OA.Permanent -> OA.Cred OA.Client -> OA.Cred OA.Permanent
oauthPermanentCred = OA.permanentCred

oserver :: OA.Server
oserver = OA.defaultServer

signRequest :: OA.Cred OA.Permanent -> OA.Server -> Client.Request -> IO Client.Request
signRequest creds server req = do
  pinx <- OA.freshPin
  let oax = OA.Oa { OA.credentials = creds, OA.workflow = OA.PermanentTokenRequest $ C8.pack "blee", OA.pin = pinx}
  return $ OA.sign oax server req

performAction :: FromJSON a => Env -> Client.Manager -> RequestProvider IO a -> IO (Either String a)
performAction env manager reqProvider = do
  req               <- getRequest reqProvider
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
  makeRequest manager signedReq
