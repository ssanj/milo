{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (runApp) where

import Control.Monad                     (void)

import Milo.Config
import Milo.Model
import Milo.Config.Model
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
import qualified Milo.Display.Tui.Main   as TUIM

runApp :: IO ()
runApp = do
  appEnv  <- getAppEnv
  manager <- Client.newManager tlsManager
  putStrLn ""
  switchUI appEnv manager

switchUI :: Env -> Client.Manager -> IO ()
switchUI appEnv manager =
  case (_uiType $ _config appEnv) of
    Console -> consoleUI appEnv manager
    TUI     -> tui appEnv manager 

consoleUI :: Env -> Client.Manager -> IO ()
consoleUI appEnv manager = do
  _dmsE <- directMessages appEnv manager
  let twitterWebUrl = _twitterWebUrl . _config $ appEnv
      directMessageResults = docToString . displayFormat twitterWebUrl $ _dmsE
  putStrLn directMessageResults >> pressAnyKeyToContinue >> void getChar
  let endpointResults = endpoints appEnv manager
      displayResults  = fmap (fmap $ docToString . displayFormat twitterWebUrl) endpointResults
  traverse_ (\x -> x >>= putStrLn >> pressAnyKeyToContinue >> getChar) displayResults

tui :: Env -> Client.Manager -> IO ()
tui appEnv manager = 
  let endpointResults :: [IO (Either TweetRetrievalError (TweetOutput [] Tweet))] = endpoints appEnv manager
  in  TUIM.main endpointResults

pressAnyKeyToContinue :: IO ()
pressAnyKeyToContinue = putStrLn "press any key to continue ..."

directMessages :: Env -> Client.Manager -> TweetResultIO DirectMessage
directMessages env manager = directMessagesAction env manager

endpoints :: Env -> Client.Manager -> [TweetResultIOWithTweet]
endpoints env manager = concatMap (\f -> f env manager) [homeTimelines, mentionsTimelines, userTimelines, searches]

homeTimelines :: Env -> Client.Manager -> [TweetResultIOWithTweet]
homeTimelines env manager = 
  if (_showHomeTimeline . _config $ env) then [homeTimelineAction env manager] else []

mentionsTimelines :: Env -> Client.Manager -> [TweetResultIOWithTweet]
mentionsTimelines env manager = 
  if (_showMentions . _config $ env) then [mentionsTimelineAction env manager] else []

userTimelines :: Env -> Client.Manager -> [TweetResultIOWithTweet]
userTimelines env manager = (userTimelineAction env manager) <$> (_userTimelines . _config $ env)

searches :: Env -> Client.Manager -> [TweetResultIOWithTweet]
searches env manager = (searchAction env manager) <$> (_searches . _config $ env)

tlsManager :: Client.ManagerSettings
tlsManager = Client.tlsManagerSettings