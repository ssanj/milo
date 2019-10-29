module Milo.DirectMessage.Controller (directMessagesAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client as Client
import Milo.DirectMessage.Service
import Milo.Format
-- import Data.Aeson (Value)

endpoint :: String
endpoint = "Direct Messages"

directMessagesAction :: Env -> Client.Manager -> IO (Either TweetRetrievalError DirectMessages)
directMessagesAction env manager = convertResults <$> getDirectMessages env manager
  where convertResults = bimap (\e -> TweetRetrievalError (Heading endpoint) (TwitterEndpoint endpoint) (TwitterError e)) (id)
