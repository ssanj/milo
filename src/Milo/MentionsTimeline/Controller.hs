module Milo.MentionsTimeline.Controller (mentionsTimelineAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client as Client
import Milo.MentionsTimeline.Service
import Milo.Format
import Data.List (intercalate)

endpoint :: String
endpoint = "Mentions Timeline"

mentionsTimelineAction :: Env -> Client.Manager -> TweetResultIO Tweet
mentionsTimelineAction env manager = convertResults <$> getMentionsTimeline env manager
  where convertResults = bimap (\e -> TweetRetrievalError (Heading endpoint) (TwitterEndpoint endpoint) (TwitterError e)) (TweetOutput (Heading endpoint) )
