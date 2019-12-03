{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Milo.Model (
    LiveSearch(..)
  , TwitterWebUrl(..)
  , RequestProvider(..)
  , TwitterHandle(..)
  , TweetCount(..)
  , SearchHitCount(..)
  , SearchCriteria(..)
  , RealName(..)
  , MentionRequest(..)
  , SearchRequest(..)
  , TweetedBy(..)
  , HeadingType(..)
  , Heading(..)
  , TweetOutput(..)
  , TweetResultIO
  , TweetResult
  , TwitterEndpoint(..)
  , TwitterError(..)
  , RetweetStatus(..)
  , TweetRetrievalError(..)
  , Tweet(..)
  , DirectMessageInfo(..)
  , DirectMessages(..)
  , DirectMessage(..)
  , TwitterSearchResult(..)
  , twitterError
) where

import GHC.Generics
import Data.Aeson (FromJSON, parseJSON, (.:), (.:?), withObject, withArray)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as Client

newtype LiveSearch = LiveSearch T.Text deriving Show

newtype TwitterWebUrl = TwitterWebUrl T.Text deriving Show

newtype RequestProvider m a = RequestProvider { getRequest :: m Client.Request }

newtype TwitterHandle = TwitterHandle T.Text deriving Show

newtype TweetCount = TweetCount Int deriving Show

newtype SearchHitCount = SearchHitCount Int deriving Show

newtype SearchCriteria = SearchCriteria T.Text deriving Show

newtype RealName = RealName T.Text deriving Show

data MentionRequest = MentionRequest RealName TwitterHandle TweetCount deriving Show

data SearchRequest = SearchRequest SearchCriteria SearchHitCount deriving Show

data TweetedBy = TweetedBy { name :: !T.Text, screen_name :: !T.Text } deriving (Generic, Show, Eq)

data HeadingType = MentionHeading | UserTimelineHeading | HomeTimelineHeading | DirectMessageHeading | SearchHeading deriving Show

data Heading = Heading HeadingType T.Text deriving Show

data TweetOutput f a = TweetOutput Heading (f a)

type TweetResultIO a = IO (Either (TweetRetrievalError) (TweetOutput [] a))

type TweetResult a = Either (TweetRetrievalError) (TweetOutput [] a)

newtype TwitterEndpoint = TwitterEndpoint T.Text deriving Show

newtype TwitterError = TwitterError T.Text deriving Show

twitterError :: String -> TwitterError
twitterError = TwitterError . T.pack

data RetweetStatus = RetweetStatus { full_text :: !T.Text, user :: TweetedBy }  deriving (Generic, Show, Eq)

data TweetRetrievalError = TweetRetrievalError Heading TwitterEndpoint TwitterError deriving Show

data Tweet = 
  Tweet { 
    created_at :: !T.Text, 
    user :: TweetedBy,
    id_str :: !T.Text,
    retweeted_status :: Maybe RetweetStatus,
    full_text :: !T.Text, 
    lang :: !T.Text
  } deriving (Generic, Show, Eq)

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
