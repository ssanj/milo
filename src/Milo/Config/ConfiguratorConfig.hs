{-# LANGUAGE OverloadedStrings #-}

module Milo.Config.ConfiguratorConfig (getConfigProvider) where

import qualified System.Environment as SYS
import qualified Milo.Model as M
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DC
import qualified Data.Text as T
import System.IO.Error (userError)
import Control.Exception.Base (ioError)

getConfigProvider :: M.ConfigProvider IO M.ConfiguratorConfig
getConfigProvider = M.ConfigProvider $ \filePrefix ->
  do
    config <- DC.load [DC.Required $ filePrefix <> ".conf"]
    M.Env <$> getMiloEnv <*> getMiloConfig config

getMiloEnv :: IO M.MiloEnv
getMiloEnv = do
  clientKey         <- M.ClientKey         <$> fromSystemEnv "clientKey"
  clientSecret      <- M.ClientSecret      <$> fromSystemEnv "clientSecret"
  accessToken       <- M.AccessToken       <$> fromSystemEnv "accessToken"
  accessTokenSecret <- M.AccessTokenSecret <$> fromSystemEnv "accessTokenSecret"
  pure $ M.MiloEnv clientKey clientSecret accessToken accessTokenSecret

fromSystemEnv :: String -> IO C8.ByteString
fromSystemEnv = fmap C8.pack . SYS.getEnv

getMiloConfig :: DC.Config -> IO M.MiloConfig
getMiloConfig config = do
  showRequest <- DC.lookupDefault False config "showRequest"
  showHomeTimeline <- DC.lookupDefault False config "showHomeTimeline"
  showMentions <- DC.lookupDefault False config "showMentions"
  userTimelines <- getUserTimelines config
  searches <- getSearches config
  return M.MiloConfig {
    M._debug = showRequest,
    M._showHomeTimeline = showHomeTimeline,
    M._showMentions = showMentions,
    M._userTimelines = userTimelines,
    M._searches = searches
  }

getUserTimelines :: DC.Config -> IO [M.MentionRequest]
getUserTimelines config = do
  userNames <- DC.lookupDefault [] config "userTimelines"
  createUserTimeline config `traverse` userNames

createUserTimeline :: DC.Config -> DC.Value -> IO M.MentionRequest
createUserTimeline config usernameValue = do
  username <- asString usernameValue
  realName <- DC.require config $ nested username "realName"
  handle   <- DC.require config $ nested username "handle"
  tweets   <- DC.lookupDefault 1 config $ nested username "tweets"
  pure $ M.MentionRequest (M.RealName realName) (M.TwitterHandle handle) (M.TweetCount tweets)

getSearches :: DC.Config -> IO [M.SearchRequest]
getSearches config = do
  searchNames <- DC.lookupDefault [] config "searches"
  createSearch config `traverse` searchNames

createSearch :: DC.Config -> DC.Value -> IO M.SearchRequest
createSearch config searchNameValue = do
  searchName <- asString searchNameValue
  searchCriteria <- DC.require config (nested searchName "term")
  hits <- DC.lookupDefault 1 config (nested searchName "hits")
  return $ M.SearchRequest (M.SearchCriteria searchCriteria) (M.SearchHitCount hits)

asString :: DC.Value -> IO T.Text
asString (DC.String value) = pure value
asString other = raiseError $ "Invalid Config Value supplied. Expected String got: " <> show other 

raiseError :: String -> IO a
raiseError error = ioError $ userError error

nested :: T.Text -> T.Text -> T.Text
nested username path = username <> "." <> path