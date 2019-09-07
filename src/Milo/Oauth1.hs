{-# LANGUAGE OverloadedStrings #-}

module Milo.Oauth1 where

import qualified Network.OAuth               as OA
import qualified Network.OAuth.Types.Params  as OA
import qualified Data.ByteString             as S
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
import Data.List (intercalate)
import Data.Aeson (FromJSON, Value, eitherDecodeStrict', Result(..), fromJSON)
import Data.Aeson.Types (parseEither)

newtype OAuthToken = OAuthToken S.ByteString
newtype OAuthTokenSecret = OAuthTokenSecret S.ByteString

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

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings

signRequest :: OA.Cred OA.Permanent -> OA.Server -> Client.Request -> IO Client.Request
signRequest creds server req = do
  pinx <- OA.freshPin
  let oax = OA.Oa { OA.credentials = creds, OA.workflow = OA.PermanentTokenRequest $ C8.pack "blee", OA.pin = pinx}
  return $ OA.sign oax server req

makeRequest :: Client.Manager -> Client.Request -> IO (Either String Value)
makeRequest manager req = do
    -- print req
    putStrLn $ maybe "-" show $ listToMaybe . filter (\(n, _) -> n == hAuthorization) . Client.requestHeaders $ req
    resp <- Client.httpLbs req manager
    -- print resp
    return $ eitherDecodeStrict' (LS.toStrict $ Client.responseBody resp)


