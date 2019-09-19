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
  traverse_ (>>= putStrLn) displayResults
  -- putStrLn "----------------------"
  -- mentionsTimelineAction env manager

endpoints :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
endpoints env manager = concatMap (\f -> f env manager) [homeTimelines, mentionsTimelines, userTimelines, searches]

-- endpoints :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
-- endpoints env manager = concatMap (\f -> f env manager) [searches]

homeTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
homeTimelines env manager = [homeTimelineAction env manager]

mentionsTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
mentionsTimelines env manager = [mentionsTimelineAction env manager]

userTimelines :: Env -> Client.Manager -> [IO (Either TweetRetrievalError TweetOutput)]
userTimelines env manager = 
  let twitterHandles = [
                         MentionRequest (TwitterHandle "wjlow") (TweetCount 5), 
                         MentionRequest (TwitterHandle "KenScambler") (TweetCount 10), 
                         MentionRequest (TwitterHandle "cwmyers") (TweetCount 2),
                         MentionRequest (TwitterHandle "ajfitzpatrick") (TweetCount 2),
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