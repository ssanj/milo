module Milo.MentionsTimeline.Controller (mentionsTimelineAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client as Client
import Milo.MentionsTimeline.Service
import Milo.Config.Model (Env)

endpoint :: String
endpoint = "Mentions Timeline"

mentionsTimelineAction :: Env -> Client.Manager -> TweetResultIO Tweet
mentionsTimelineAction env manager = convertResults <$> getMentionsTimeline env manager
  where 
        heading = Heading MentionHeading endpoint
        convertResults :: Either String [Tweet] -> TweetResult Tweet
        convertResults = bimap (\e -> TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError e)) 
                               (TweetOutput heading)
