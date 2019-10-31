{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Milo.Config
import Milo.Model
import Data.Foldable                     (traverse_)
import Milo.Format                       (displayString, displayJson, displayDirectMessageString)
import Milo.HomeTimeline.Controller      (homeTimelineAction)
import Milo.MentionsTimeline.Controller  (mentionsTimelineAction)
import Milo.DirectMessage.Controller     (directMessagesAction)
import Milo.UserTimeline.Controller      (userTimelineAction)
import Milo.Search.Controller            (searchAction)
import qualified Network.HTTP.Client     as Client
import qualified Network.HTTP.Client.TLS as Client
import Data.Aeson (Value)
import Data.List (intercalate)
import Control.Monad (void)

someFunc :: IO ()
someFunc = do
  appEnv  <- getAppEnv
  manager <- Client.newManager tlsManager
  putStrLn ""
  _dmsE <- directMessages appEnv manager
  let twitterWebUrl = _twitterWebUrl . _config $ appEnv
      directMessageResults = displayDirectMessageString twitterWebUrl _dmsE
  putStrLn directMessageResults >> pressAnyKeyToContinue >> void getChar
  let endpointResults = endpoints appEnv manager
      displayResults  = fmap (fmap $ displayString twitterWebUrl) endpointResults
  traverse_ (\x -> x >>= putStrLn >> pressAnyKeyToContinue >> getChar) displayResults

pressAnyKeyToContinue :: IO ()
pressAnyKeyToContinue = putStrLn "press any key to continue ..."

directMessages :: Env -> Client.Manager -> IO (Either TweetRetrievalError DirectMessages)
directMessages env manager = directMessagesAction env manager

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