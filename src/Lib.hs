{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Milo.Config
import Milo.Model
import Data.Foldable                     (traverse_)
import Milo.Format                       (displayString)
import Milo.HomeTimeline.Controller      (homeTimelineAction)
import Milo.MentionsTimeline.Controller  (mentionsTimelineAction)
import Milo.UserTimeline.Controller      (userTimelineAction)
import Milo.Search.Controller            (searchAction)
import qualified Network.HTTP.Client     as Client
import qualified Network.HTTP.Client.TLS as Client

someFunc :: IO ()
someFunc = do
  appEnv  <- getAppEnv
  manager <- Client.newManager tlsManager
  putStrLn ""
  let endpointResults = endpoints appEnv manager
      displayResults = fmap (fmap displayString) endpointResults
  traverse_ (\x -> x >>= putStrLn >> pressAnyKeyToContinue >> getChar) displayResults

pressAnyKeyToContinue :: IO ()
pressAnyKeyToContinue = putStrLn "press any key to continue ..."

endpoints :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
endpoints env manager = concatMap (\f -> f env manager) [homeTimelines, mentionsTimelines, userTimelines, searches]

homeTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
homeTimelines env manager = 
  if (_showHomeTimeline . _config $ env) then [homeTimelineAction env manager] else []

mentionsTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
mentionsTimelines env manager = 
  if (_showMentions . _config $ env) then [mentionsTimelineAction env manager] else []

userTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
userTimelines env manager = (userTimelineAction env manager) <$> (_userTimelines . _config $ env)

searches :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
searches env manager = (searchAction env manager) <$> (_searches . _config $ env)

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings