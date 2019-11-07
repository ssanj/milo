module Milo.DirectMessage.Controller (directMessagesAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client as Client
import Milo.DirectMessage.Service
import Milo.Config.Model (Env)

endpoint :: String
endpoint = "Direct Messages"

directMessagesAction :: Env -> Client.Manager -> TweetResultIO DirectMessage
directMessagesAction env manager = convertResults <$> getDirectMessages env manager
  where
        heading = Heading DirectMessageHeading endpoint
        convertResults :: Either String DirectMessages -> TweetResult DirectMessage
        convertResults = bimap (\e -> TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError e)) (TweetOutput heading . messages)
