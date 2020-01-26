{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Milo.Config.YamlConfig (getConfigProvider) where

import qualified System.Environment as SYS
import qualified Milo.Model as M
import qualified Milo.Config.Model as CM
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import System.IO.Error (userError)
import Control.Exception.Base (ioError)
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import qualified Data.Aeson.Casing as A
import GHC.Generics

data MiloYamlConfig = MiloYamlConfig {
    miloyamlconfigShowRequest :: Bool
  , miloyamlconfigShowHomeTimeline :: Bool
  , miloyamlconfigShowMentions :: Bool
  , miloyamlconfigShowDirectMessages :: Bool
  , miloyamlconfigUserTimelines :: [MiloYamlUser]
  , miloyamlconfigSearches :: [MiloYamlSearch]
  , miloyamlconfigTwitterWebUrl :: T.Text
  , miloyamlconfigUi :: T.Text
} deriving (Generic, Show)

data MiloYamlUser = MiloYamlUser {
  miloyamluserRealName :: T.Text,
  miloyamluserHandle :: T.Text,
  miloyamluserTweets :: Int
} deriving (Generic, Show)

data MiloYamlSearch = MiloYamlSearch {
  miloyamlsearchTitle :: T.Text,
  miloyamlsearchTerm :: T.Text,
  miloyamlsearchHits :: Int
} deriving (Generic, Show)

getConfigProvider :: CM.ConfigProvider IO CM.YamlConfig
getConfigProvider = CM.ConfigProvider $ \filePrefix ->
  do
    configE <- Y.decodeFileEither $ filePrefix <> ".yaml" 
    case configE of 
      Left configError -> raiseError $ "got an error loading yaml: " <> (show configError)
      Right config -> (CM.Env <$> getMiloEnv) `blah` getMiloConfig config

blah :: Applicative f => f (a -> b) -> a -> f b
blah fab a = fab <*> pure a

getMiloEnv :: IO CM.MiloEnv
getMiloEnv = do
  clientKey         <- CM.ClientKey         <$> fromSystemEnv "clientKey"
  clientSecret      <- CM.ClientSecret      <$> fromSystemEnv "clientSecret"
  accessToken       <- CM.AccessToken       <$> fromSystemEnv "accessToken"
  accessTokenSecret <- CM.AccessTokenSecret <$> fromSystemEnv "accessTokenSecret"
  pure $ CM.MiloEnv clientKey clientSecret accessToken accessTokenSecret

fromSystemEnv :: String -> IO C8.ByteString
fromSystemEnv = fmap C8.pack . SYS.getEnv

getMiloConfig :: MiloYamlConfig  -> CM.MiloConfig
getMiloConfig config = 
  let showRequest        = miloyamlconfigShowRequest config
      showHomeTimeline   = miloyamlconfigShowHomeTimeline config
      showMentions       = miloyamlconfigShowMentions config
      showDirectMessages = miloyamlconfigShowDirectMessages config
      userTimelines      = getUserTimelines config
      searches           = getSearches config
      twitterWebUrl      = miloyamlconfigTwitterWebUrl config
      uiType             = miloyamlconfigUi config
  in CM.MiloConfig {
         CM._debug              = showRequest
       , CM._showHomeTimeline   = showHomeTimeline
       , CM._showMentions       = showMentions
       , CM._showDirectMessages = showDirectMessages
       , CM._userTimelines      = userTimelines
       , CM._searches           = searches
       , CM._twitterWebUrl      = M.TwitterWebUrl twitterWebUrl
       , CM._uiType             = M.parseUiType uiType
     }

instance A.FromJSON MiloYamlConfig where
  parseJSON = A.genericParseJSON $ A.aesonPrefix A.trainCase

instance A.FromJSON MiloYamlUser where
  parseJSON = A.genericParseJSON $ A.aesonPrefix A.trainCase

instance A.FromJSON MiloYamlSearch where
  parseJSON = A.genericParseJSON $ A.aesonPrefix A.trainCase

getUserTimelines :: MiloYamlConfig -> [M.MentionRequest]
getUserTimelines config = 
  (\(MiloYamlUser realName handle tweets) -> M.MentionRequest (M.RealName realName) (M.TwitterHandle handle) (M.TweetCount tweets)) <$> miloyamlconfigUserTimelines config

getSearches :: MiloYamlConfig -> [M.SearchRequest]
getSearches config = 
  (\(MiloYamlSearch _ term hits) -> M.SearchRequest (M.SearchCriteria term) (M.SearchHitCount hits)) <$> miloyamlconfigSearches config

raiseError :: String -> IO a
raiseError errorMessage = ioError $ userError errorMessage

