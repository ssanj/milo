module Milo.HomeTimeline.Controller (homeTimelineAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client as Client
import Milo.HomeTimeline.Service
import Milo.Config.Model (Env)

endpoint :: String
endpoint = "Home Timeline"

homeTimelineAction :: Env -> Client.Manager -> TweetResultIO Tweet
homeTimelineAction env manager = convertResults <$> getHomeTimeline env manager
  where 
        heading = Heading HomeTimelineHeading endpoint
        convertResults :: Either String [Tweet] -> TweetResult Tweet
        convertResults = bimap (\e -> TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError e)) 
                           (TweetOutput heading)
