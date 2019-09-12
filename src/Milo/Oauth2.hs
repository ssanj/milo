module Milo.Oauth2 where

import qualified Network.OAuth               as OA
import qualified Network.OAuth.Types.Params  as OA
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as LS
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import Network.HTTP.Types.Header (hAuthorization)
import Data.CaseInsensitive (mk)
import Milo.Config
import Milo.Model
import Milo.Oauth2.Model
import Data.Aeson (FromJSON, Value, eitherDecodeStrict', Result(..), fromJSON)
import Data.Aeson.Types (parseEither)

-- TODO: this is common and should be reused.
makeRequest :: FromJSON a => Client.Manager -> Client.Request -> IO (Either String a)
makeRequest manager req = do
    resp <- Client.httpLbs req manager
    return $ eitherDecodeStrict' (LS.toStrict $ Client.responseBody resp)

performAction :: (FromJSON a, FromJSON b) => Env -> Client.Manager -> (OAuth2ClientToken -> RequestProvider IO a) -> (a -> RequestProvider IO b) -> IO (Either String b)
performAction env manager reqProviderA reqProviderB = do
  let 
      clientKey         = _clientKey $ env
      clientSecret      = _clientSecret $ env
      clientToken       = OAuth2ClientToken clientKey clientSecret
  tokenResponseE <- (getRequest $ reqProviderA clientToken) >>= makeRequest manager
  case tokenResponseE of
    Right bearer -> (getRequest $ reqProviderB bearer) >>= makeRequest manager
    Left error -> pure . Left $ error

tap :: Show a => IO a -> (a -> IO ()) -> IO a
tap ioa f = ioa >>= f >> ioa
