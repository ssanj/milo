{-# LANGUAGE OverloadedStrings #-}

module Milo.Oauth1.Controller (performAction) where

import qualified Network.OAuth               as OA
import qualified Network.OAuth.Types.Params  as OA
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import Milo.Model
import Milo.Config.Model
import Milo.Request (makeRequest)
import Milo.Oauth1.Model
import Data.Aeson (FromJSON)

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
      clientKey         = unClientKey . _clientKey . _env $ env
      clientSecret      = unClientSecret . _clientSecret . _env $ env
      accessToken       = unAccessToken . _accessToken . _env $ env
      accessTokenSecret = unAccessTokenSecret . _accessTokenSecret . _env $ env
      clientToken       = clientOauthToken (OAuthToken clientKey) (OAuthTokenSecret clientSecret)
      clientCred        = oauthClientCred clientToken
      permToken         = permanentOauthToken (OAuthToken accessToken) (OAuthTokenSecret accessTokenSecret)
      permCred          = oauthPermanentCred permToken clientCred
      config            = _config env
  signedReq <- signRequest permCred oserver req
  makeRequest config manager signedReq
