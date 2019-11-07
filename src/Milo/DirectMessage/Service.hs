{-# LANGUAGE OverloadedStrings #-}

module Milo.DirectMessage.Service (getDirectMessages) where

import Milo.Oauth1.Controller
import Milo.Model
import Milo.Config.Model (Env)

import qualified Network.HTTP.Client         as Client

getDirectMessages :: Env -> Client.Manager -> IO (Either String DirectMessages)
getDirectMessages env manager = performAction env manager $ RequestProvider $ Client.parseRequest directMessagesUrl

directMessagesUrl :: String
directMessagesUrl = "https://api.twitter.com/1.1/direct_messages/events/list.json"
