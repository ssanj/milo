{-# LANGUAGE OverloadedStrings #-}

module Lib where

      -- import Data.Coerce (coerce)
      -- import Control.Monad.Reader (ReaderT, MonadReader)
      -- import Control.Monad.IO.Class (MonadIO)
      -- import Data.Functor
      -- import Control.Applicative

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
import Boot
import Model
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

oauthUrl :: String
oauthUrl = "https://api.twitter.com/1.1/statuses/home_timeline.json"

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

getHomeTimeline :: IO (Either String [HomeTimeline])
getHomeTimeline = do
  env               <- getConfig
  req               <- Client.parseRequest oauthUrl
  manager           <- Client.newManager tlsManager
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

formatHomeTimeline :: HomeTimeline -> String
formatHomeTimeline (HomeTimeline created_at (HomeTimeLineUser name screen_name) text lang) =
  text <> " - @" <> screen_name <> " on " <> created_at
    
someFunc :: IO ()
someFunc = do
  result <- getHomeTimeline
  let output = case result of
                Left error -> "Couldn't get Home Timeline due to " <> error
                Right hometimeline -> intercalate "\n" $ (\(n, v) -> (show n) <> ". " <> formatHomeTimeline v) <$> (zip [1..] hometimeline)
  putStrLn output

