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
  env     <- getMiloEnv
  manager <- Client.newManager tlsManager
  let config = getMiloConfig
  putStrLn ""
  let endpointResults = endpoints env config manager
      displayResults = fmap (fmap displayString) endpointResults
  traverse_ (\x -> x >>= putStrLn >> pressAnyKeyToContinue >> getChar) displayResults

pressAnyKeyToContinue :: IO ()
pressAnyKeyToContinue = putStrLn "press any key to continue ..."

endpoints :: Env -> MiloConfig -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
endpoints env config manager = concatMap (\f -> f env config manager) [homeTimelines, mentionsTimelines, userTimelines, searches]

homeTimelines :: Env -> MiloConfig -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
homeTimelines env config manager = 
  if (_showHomeTimeline config) then [homeTimelineAction env manager] else []

mentionsTimelines :: Env -> MiloConfig -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
mentionsTimelines env config manager = 
  if (_showMentions config) then [mentionsTimelineAction env manager] else []

userTimelines :: Env -> MiloConfig -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
userTimelines env config manager = (userTimelineAction env manager) <$> (_userTimelines config)

searches :: Env -> MiloConfig -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
searches env config manager = (searchAction env manager) <$> (_searches config)

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings