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
  env     <- getConfig
  manager <- Client.newManager tlsManager
  putStrLn ""
  let endpointResults = endpoints env manager
      displayResults = fmap (fmap displayString) endpointResults
  traverse_ (\x -> x >>= putStrLn >> pressAnyKeyToContinue >> getChar) displayResults

pressAnyKeyToContinue :: IO ()
pressAnyKeyToContinue = putStrLn "press any key to continue ..."

endpoints :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
endpoints env manager = concatMap (\f -> f env manager) [homeTimelines, mentionsTimelines, userTimelines, searches]

homeTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
homeTimelines env manager = [homeTimelineAction env manager]

mentionsTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
mentionsTimelines env manager = [mentionsTimelineAction env manager]

userTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
userTimelines env manager = 
  let twitterHandles = [
                         MentionRequest (TwitterHandle "wjlow") (TweetCount 10), 
                         MentionRequest (TwitterHandle "KenScambler") (TweetCount 15), 
                         MentionRequest (TwitterHandle "cwmyers") (TweetCount 5),
                         MentionRequest (TwitterHandle "ajfitzpatrick") (TweetCount 5),
                         MentionRequest (TwitterHandle "andrewfnewman") (TweetCount 5)
                       ]
  in (userTimelineAction env manager) <$> twitterHandles

searches :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
searches env manager = 
  let searches = [
                   SearchRequest (SearchCriteria "#scala") (SearchHitCount 10),
                   SearchRequest (SearchCriteria "#haskell") (SearchHitCount 10)
                 ]
  in (searchAction env manager) <$> searches

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings