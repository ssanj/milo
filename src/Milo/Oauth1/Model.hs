module Milo.Oauth1.Model where

import qualified Data.ByteString             as S

newtype OAuthToken = OAuthToken S.ByteString
newtype OAuthTokenSecret = OAuthTokenSecret S.ByteString
