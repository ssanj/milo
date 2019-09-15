{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Milo.Config
import Milo.Model
import Milo.HomeTimeline.Controller
import Milo.MentionsTimeline.Controller
import Milo.UserTimeline.Controller
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client

someFunc :: IO ()
someFunc = do
  env     <- getConfig
  manager <- Client.newManager tlsManager
  userTimelineAction env manager $ TwitterHandle "wjlow"
  userTimelineAction env manager $ TwitterHandle "KenScambler"
  userTimelineAction env manager $ TwitterHandle "cwmyers"
  homeTimelineAction env manager
  putStrLn "----------------------"
  mentionsTimelineAction env manager

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings