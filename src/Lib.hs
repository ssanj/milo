module Lib where

import Milo.Config
import Milo.HomeTimeline.Controller
import Milo.MentionsTimeline.Controller
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client

someFunc :: IO ()
someFunc = do
  env     <- getConfig
  manager <- Client.newManager tlsManager
  homeTimelineAction env manager
  putStrLn "----------------------"
  mentionsTimelineAction env manager

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings