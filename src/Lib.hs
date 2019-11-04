{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Milo.Config
import Milo.Model
import Data.Foldable                     (traverse_)
import Milo.Format.Format                (docToString)
import Milo.Format.Model                 (displayFormat)
import Milo.HomeTimeline.Controller      (homeTimelineAction)
import Milo.MentionsTimeline.Controller  (mentionsTimelineAction)
import Milo.DirectMessage.Controller     (directMessagesAction)
import Milo.UserTimeline.Controller      (userTimelineAction)
import Milo.Search.Controller            (searchAction)
import qualified Network.HTTP.Client     as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
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
      directMessageResults = docToString . displayFormat twitterWebUrl $ _dmsE
  putStrLn directMessageResults >> pressAnyKeyToContinue >> void getChar
  let endpointResults = endpoints appEnv manager
      displayResults  = fmap (fmap $ docToString . displayFormat twitterWebUrl) endpointResults
  traverse_ (\x -> x >>= putStrLn >> pressAnyKeyToContinue >> getChar) displayResults

pressAnyKeyToContinue :: IO ()
pressAnyKeyToContinue = putStrLn "press any key to continue ..."

directMessages :: Env -> Client.Manager -> TweetResultIO DirectMessage
directMessages env manager = directMessagesAction env manager

endpoints :: Env -> Client.Manager -> [TweetResultIO Tweet]
endpoints env manager = concatMap (\f -> f env manager) [homeTimelines, mentionsTimelines, userTimelines, searches]

homeTimelines :: Env -> Client.Manager -> [TweetResultIO Tweet]
homeTimelines env manager = 
  if (_showHomeTimeline . _config $ env) then [homeTimelineAction env manager] else []

mentionsTimelines :: Env -> Client.Manager -> [TweetResultIO Tweet]
mentionsTimelines env manager = 
  if (_showMentions . _config $ env) then [mentionsTimelineAction env manager] else []

userTimelines :: Env -> Client.Manager -> [TweetResultIO Tweet]
userTimelines env manager = (userTimelineAction env manager) <$> (_userTimelines . _config $ env)

searches :: Env -> Client.Manager -> [TweetResultIO Tweet]
searches env manager = (searchAction env manager) <$> (_searches . _config $ env)

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings