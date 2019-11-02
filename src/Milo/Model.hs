{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Milo.Model where

import GHC.Generics
import Data.Aeson (FromJSON, parseJSON, (.:), (.:?), withObject, withArray)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client   as Client
-- import Data.Functor.Identity (Identity)

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

newtype LiveSearch = LiveSearch String deriving Show

newtype TwitterWebUrl = TwitterWebUrl T.Text deriving Show

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

newtype RequestProvider m a = RequestProvider { getRequest :: m Client.Request }

newtype ConfigProvider m a = ConfigProvider {
  loadConfig :: String -> m Env
}

data ConfiguratorConfig

data YamlConfig

newtype TwitterHandle = TwitterHandle T.Text deriving Show

newtype TweetCount = TweetCount Int deriving Show

newtype SearchHitCount = SearchHitCount Int deriving Show

newtype SearchCriteria = SearchCriteria T.Text deriving Show

newtype RealName = RealName T.Text deriving Show

data MentionRequest = MentionRequest RealName TwitterHandle TweetCount deriving Show

data SearchRequest = SearchRequest SearchCriteria SearchHitCount deriving Show

data TweetedBy = TweetedBy { name :: !String, screen_name :: !String } deriving (Generic, Show)

data HeadingType = Heading String | Mention String | Search String deriving Show

data TweetOutput f a = TweetOutput HeadingType (f a)

type TweetResultIO a = IO (Either TweetRetrievalError (TweetOutput [] a))

type TweetResult a = Either TweetRetrievalError (TweetOutput [] a)

newtype TwitterEndpoint = TwitterEndpoint String deriving Show

newtype TwitterError = TwitterError String deriving Show

data RetweetStatus = RetweetStatus { full_text :: !String, user :: TweetedBy }  deriving (Generic, Show)

data TweetRetrievalError = TweetRetrievalError HeadingType TwitterEndpoint TwitterError deriving Show

data Tweet = 
  Tweet { 
    created_at :: !String, 
    user :: TweetedBy,
    id_str :: !T.Text,
    retweeted_status :: Maybe RetweetStatus,
    full_text :: !String, 
    lang :: !String
  } deriving (Generic, Show)

data DirectMessageInfo = DirectMessageInfo {
  source_app_id :: Maybe T.Text, 
  recipient_id :: !T.Text,
  sender_id :: !T.Text,
  text :: !T.Text
} deriving Show

data DirectMessage =
  DirectMessage {
    created_at :: !T.Text,
    id_str :: !T.Text,
    message_info :: DirectMessageInfo,
    message_type :: !T.Text
} deriving Show 

data DirectMessages = DirectMessages { messages :: [DirectMessage] } deriving Show

newtype TwitterSearchResult = TwitterSearchResult { statuses :: [Tweet] } deriving (Generic, Show)

instance FromJSON TweetedBy where
instance FromJSON Tweet where
instance FromJSON TwitterSearchResult where
instance FromJSON RetweetStatus where

instance FromJSON DirectMessage where
  parseJSON = withObject "direct message" $ \v -> 
    do
      _created_at    <- v .: "created_timestamp"
      _id_str        <- v .: "id"
      _message_type  <- v .: "type"
      messageCreate  <- v .: "message_create"
      _source_app_id <- messageCreate .:? "source_app_id"
      _sender_id     <- messageCreate .: "sender_id"
      target         <- messageCreate .: "target"
      _recipient_id  <- target .: "recipient_id"
      messageData    <- messageCreate .: "message_data"
      _text          <- messageData .: "text"
      pure DirectMessage {
        created_at = _created_at ,
        id_str = _id_str, 
        message_info = 
          DirectMessageInfo {
            source_app_id = _source_app_id,
            recipient_id = _recipient_id,
            sender_id = _sender_id,
            text = _text
          },
        message_type = _message_type
      }

instance FromJSON DirectMessages where
  parseJSON = withObject "direct messages" $ \o ->
    do
      events <- o .: "events"
      withArray "events" (fmap DirectMessages . fmap V.toList . traverse parseJSON) events
