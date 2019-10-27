{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Milo.Config.YamlConfig (getConfigProvider) where

import qualified System.Environment as SYS
import qualified Milo.Model as M
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import System.IO.Error (userError)
import Control.Exception.Base (ioError)
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import qualified Data.Aeson.Casing as A
import GHC.Generics

data MiloYamlConfig = MiloYamlConfig {
  miloyamlconfigShowRequest :: Bool,
  miloyamlconfigShowHomeTimeline :: Bool,
  miloyamlconfigShowMentions :: Bool,
  miloyamlconfigUserTimelines :: [MiloYamlUser],
  miloyamlconfigSearches :: [MiloYamlSearch],
  miloyamlconfigTwitterWebUrl :: T.Text
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

getConfigProvider :: M.ConfigProvider IO M.YamlConfig
getConfigProvider = M.ConfigProvider $ \filePrefix ->
  do
    configE <- Y.decodeFileEither $ filePrefix <> ".yaml" 
    case configE of 
      Left error -> raiseError $ "got an error loading yaml: " <> (show error)
      Right config -> (M.Env <$> getMiloEnv) `blah` getMiloConfig config

blah :: Applicative f => f (a -> b) -> a -> f b
blah fab a = fab <*> pure a

getMiloEnv :: IO M.MiloEnv
getMiloEnv = do
  clientKey         <- M.ClientKey         <$> fromSystemEnv "clientKey"
  clientSecret      <- M.ClientSecret      <$> fromSystemEnv "clientSecret"
  accessToken       <- M.AccessToken       <$> fromSystemEnv "accessToken"
  accessTokenSecret <- M.AccessTokenSecret <$> fromSystemEnv "accessTokenSecret"
  pure $ M.MiloEnv clientKey clientSecret accessToken accessTokenSecret

fromSystemEnv :: String -> IO C8.ByteString
fromSystemEnv = fmap C8.pack . SYS.getEnv

getMiloConfig :: MiloYamlConfig  -> M.MiloConfig
getMiloConfig config = 
  let showRequest      = miloyamlconfigShowRequest config
      showHomeTimeline = miloyamlconfigShowHomeTimeline config
      showMentions     = miloyamlconfigShowMentions config
      userTimelines    = getUserTimelines config
      searches         = getSearches config
      twitterWebUrl    = miloyamlconfigTwitterWebUrl config
  in M.MiloConfig {
       M._debug = showRequest,
       M._showHomeTimeline = showHomeTimeline,
       M._showMentions = showMentions,
       M._userTimelines = userTimelines,
       M._searches = searches,
       M._twitterWebUrl = M.TwitterWebUrl twitterWebUrl
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
raiseError error = ioError $ userError error

