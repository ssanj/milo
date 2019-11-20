module Milo.Config.Model (
    ClientKey(..)
  , ClientSecret(..)
  , AccessToken(..)
  , AccessTokenSecret(..)
  , Env(..)
  , MiloEnv(..)
  , MiloConfig(..)
  , ConfigProvider(..)
  , YamlConfig
  , debugSet
) where

import Milo.Model (MentionRequest)
import Milo.Model (SearchRequest)
import Milo.Model (TwitterWebUrl)

import qualified Data.ByteString.Char8 as C8

newtype ClientKey         = ClientKey { unClientKey :: C8.ByteString } deriving Show
newtype ClientSecret      = ClientSecret { unClientSecret :: C8.ByteString } deriving Show
newtype AccessToken       = AccessToken { unAccessToken :: C8.ByteString } deriving Show
newtype AccessTokenSecret = AccessTokenSecret { unAccessTokenSecret :: C8.ByteString } deriving Show

data Env = Env {
  _env :: MiloEnv,
  _config :: MiloConfig
}

data MiloEnv = MiloEnv { 
  _clientKey :: ClientKey, 
  _clientSecret :: ClientSecret, 
  _accessToken :: AccessToken, 
  _accessTokenSecret :: AccessTokenSecret
} deriving Show

-- Boolean blindness?
data MiloConfig = MiloConfig{
  _debug :: Bool,
  _showHomeTimeline :: Bool,
  _showMentions :: Bool,
  _showDirectMessages :: Bool,
  _userTimelines :: [MentionRequest],
  _searches :: [SearchRequest],
  _twitterWebUrl :: TwitterWebUrl
} deriving Show

debugSet :: Env -> Bool
debugSet = _debug . _config

newtype ConfigProvider m a = ConfigProvider {
  loadConfig :: String -> m Env
}

data YamlConfig