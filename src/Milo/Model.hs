module Milo.Model where

import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Client   as Client

newtype ClientKey         = ClientKey { unClientKey :: C8.ByteString }
newtype ClientSecret      = ClientSecret { unClientSecret :: C8.ByteString }
newtype AccessToken       = AccessToken { unAccessToken :: C8.ByteString }
newtype AccessTokenSecret = AccessTokenSecret { unAccessTokenSecret :: C8.ByteString }

data Env = Env { _clientKey :: ClientKey, _clientSecret :: ClientSecret, _accessToken :: AccessToken, _accessTokenSecret :: AccessTokenSecret}

newtype RequestProvider m a = RequestProvider { getRequest :: m Client.Request }
