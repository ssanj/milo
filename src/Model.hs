{-# LANGUAGE DeriveGeneric #-}

module Model where

import GHC.Generics
import qualified Data.ByteString.Char8 as C8
import Data.Aeson (FromJSON)

newtype ClientKey         = ClientKey { unClientKey :: C8.ByteString }
newtype ClientSecret      = ClientSecret { unClientSecret :: C8.ByteString }
newtype AccessToken       = AccessToken { unAccessToken :: C8.ByteString }
newtype AccessTokenSecret = AccessTokenSecret { unAccessTokenSecret :: C8.ByteString }

data Env = Env { _clientKey :: ClientKey, _clientSecret :: ClientSecret, _accessToken :: AccessToken, _accessTokenSecret :: AccessTokenSecret}

data HomeTimeLineUser = HomeTimeLineUser { name :: !String, screen_name :: !String } deriving (Generic, Show)

data HomeTimeline = 
  HomeTimeline { 
    created_at :: !String, 
    user :: HomeTimeLineUser,
    text :: !String, 
    lang :: !String
  } deriving (Generic, Show)  

instance FromJSON HomeTimeLineUser where
instance FromJSON HomeTimeline where