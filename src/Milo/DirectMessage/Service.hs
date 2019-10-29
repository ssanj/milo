{-# LANGUAGE OverloadedStrings #-}

module Milo.DirectMessage.Service (getDirectMessages) where

import Milo.Oauth1.Controller
import Milo.Config
import Milo.Model
import Milo.Oauth1.Model

import qualified Data.Text.Encoding          as T
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import qualified Network.HTTP.Types          as Client
-- import Data.Aeson (Value)

getDirectMessages :: Env -> Client.Manager -> IO (Either String DirectMessages)
getDirectMessages env manager = performAction env manager $ RequestProvider $ Client.parseRequest directMessagesUrl

directMessagesUrl :: String
directMessagesUrl = "https://api.twitter.com/1.1/direct_messages/events/list.json"
