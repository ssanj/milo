{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Milo.Config
import Milo.Model
import Data.Foldable (traverse_)
import Milo.Format (displayString)
import Milo.HomeTimeline.Controller
import Milo.MentionsTimeline.Controller
import Milo.UserTimeline.Controller
import qualified Network.HTTP.Client as Client
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
endpoints env manager = 
  let twitterHandles = [TwitterHandle "wjlow", TwitterHandle "KenScambler", TwitterHandle "cwmyers"]
  in (userTimelineAction env manager) <$> twitterHandles

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings