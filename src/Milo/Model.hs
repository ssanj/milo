{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Milo.Model where

import GHC.Generics
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Client   as Client

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

data LiveSearch = LiveSearch !String deriving Show

-- Boolean blindness?
data MiloConfig = MiloConfig{
  _debug :: Bool,
  _showHomeTimeline :: Bool,
  _showMentions :: Bool,
  _userTimelines :: [MentionRequest],
  _searches :: [SearchRequest]
} deriving Show

newtype RequestProvider m a = RequestProvider { getRequest :: m Client.Request }

newtype TwitterHandle = TwitterHandle C8.ByteString deriving Show

newtype TweetCount = TweetCount Int deriving Show

newtype SearchHitCount = SearchHitCount Int deriving Show

newtype SearchCriteria = SearchCriteria C8.ByteString deriving Show

data MentionRequest = MentionRequest TwitterHandle TweetCount deriving Show

data SearchRequest = SearchRequest SearchCriteria SearchHitCount deriving Show

data TweetedBy = TweetedBy { name :: !String, screen_name :: !String } deriving (Generic, Show)

data HeadingType = Heading String | Mention String | Search String

data TweetOutput = TweetOutput HeadingType [Tweet]

newtype TwitterEndpoint = TwitterEndpoint String

newtype TwitterError = TwitterError String

data RetweetStatus = RetweetStatus { full_text :: !String, user :: TweetedBy }  deriving (Generic, Show)

data TweetRetrievalError = TweetRetrievalError HeadingType TwitterEndpoint TwitterError

data Tweet = 
  Tweet { 
    created_at :: !String, 
    user :: TweetedBy,
    retweeted_status :: Maybe RetweetStatus,
    full_text :: !String, 
    lang :: !String
  } deriving (Generic, Show)

data TwitterSearchResult = TwitterSearchResult { statuses :: [Tweet] } deriving (Generic, Show)

instance FromJSON TweetedBy where
instance FromJSON Tweet where
instance FromJSON TwitterSearchResult where
instance FromJSON RetweetStatus where