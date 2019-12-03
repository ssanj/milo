{-# LANGUAGE OverloadedStrings #-}

module Milo.DirectMessage.Controller (directMessagesAction) where

import Milo.Model
import Milo.DirectMessage.Service

import Data.Bifunctor (bimap)
import qualified Data.Text as T

import Milo.Config.Model (Env)

import qualified Network.HTTP.Client as Client

endpoint :: T.Text
endpoint = "Direct Messages"

directMessagesAction :: Env -> Client.Manager -> TweetResultIO DirectMessage
directMessagesAction env manager = convertResults <$> getDirectMessages env manager
  where
        heading = Heading DirectMessageHeading endpoint
        convertResults :: Either String DirectMessages -> TweetResult DirectMessage
        convertResults = bimap (\e -> TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError $ T.pack e)) (TweetOutput heading . messages)
