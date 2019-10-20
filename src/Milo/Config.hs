{-# LANGUAGE OverloadedStrings #-}
module Milo.Config (getAppEnv) where

import qualified System.Environment as SYS
import qualified Milo.Model as M
import Control.Monad
import qualified Data.ByteString.Char8 as C8

getAppEnv :: IO M.Env
getAppEnv = M.Env <$> getMiloEnv <*> (pure getMiloConfig)

getMiloEnv :: IO M.MiloEnv
getMiloEnv = do
  clientKey         <- M.ClientKey         <$> fromSystemEnv "clientKey"
  clientSecret      <- M.ClientSecret      <$> fromSystemEnv "clientSecret"
  accessToken       <- M.AccessToken       <$> fromSystemEnv "accessToken"
  accessTokenSecret <- M.AccessTokenSecret <$> fromSystemEnv "accessTokenSecret"
  pure $ M.MiloEnv clientKey clientSecret accessToken accessTokenSecret

fromSystemEnv :: String -> IO C8.ByteString
fromSystemEnv = fmap C8.pack . SYS.getEnv

getMiloConfig :: M.MiloConfig
getMiloConfig = 
  M.MiloConfig {
    M._debug = False,
    M._showHomeTimeline = False,
    M._showMentions = False,
    M._userTimelines = [
      M.MentionRequest (M.TwitterHandle "wjlow")         (M.TweetCount 10), 
      M.MentionRequest (M.TwitterHandle "KenScambler")   (M.TweetCount 15)--, 
      -- M.MentionRequest (M.TwitterHandle "cwmyers")       (M.TweetCount 5),
      -- M.MentionRequest (M.TwitterHandle "ajfitzpatrick") (M.TweetCount 5),
      -- M.MentionRequest (M.TwitterHandle "andrewfnewman") (M.TweetCount 5)
    ],
    M._searches = [
      M.SearchRequest (M.SearchCriteria "#scala")   (M.SearchHitCount 2),
      M.SearchRequest (M.SearchCriteria "#haskell") (M.SearchHitCount 2)
    ]
  }