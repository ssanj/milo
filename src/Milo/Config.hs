{-# LANGUAGE OverloadedStrings #-}
module Milo.Config (getMiloEnv, getMiloConfig) where

import System.Environment (getEnv)
import qualified Milo.Model as M
import Control.Monad
import qualified Data.ByteString.Char8 as C8

getMiloEnv :: IO M.Env
getMiloEnv = do
  clientKey         <- M.ClientKey         <$> fromEnv "clientKey"
  clientSecret      <- M.ClientSecret      <$> fromEnv "clientSecret"
  accessToken       <- M.AccessToken       <$> fromEnv "accessToken"
  accessTokenSecret <- M.AccessTokenSecret <$> fromEnv "accessTokenSecret"
  pure $ M.Env clientKey clientSecret accessToken accessTokenSecret

fromEnv :: String -> IO C8.ByteString
fromEnv = fmap C8.pack . getEnv

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