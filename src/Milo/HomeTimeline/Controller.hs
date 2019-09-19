module Milo.HomeTimeline.Controller (homeTimelineAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client as Client
import Milo.HomeTimeline.Service
import Milo.Format
import Data.List (intercalate)

endpoint :: String
endpoint = "Home Timeline"

homeTimelineAction :: Env -> Client.Manager -> IO (Either TweetRetrievalError TweetOutput)
homeTimelineAction env manager = convertResults <$> getHomeTimeline env manager
  where convertResults = bimap (\e -> TweetRetrievalError (Heading endpoint) (TwitterEndpoint endpoint) (TwitterError e)) (TweetOutput (Heading endpoint) )
