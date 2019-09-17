module Milo.UserTimeline.Controller (userTimelineAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client as Client
import qualified Data.ByteString.Char8       as C8
import Milo.UserTimeline.Service
import Milo.Format
import Data.List (intercalate)

endpoint :: String
endpoint = "User Timeline"

userTimelineAction :: Env -> Client.Manager -> MentionRequest -> IO (Either TweetRetrievalError TweetOutput)
userTimelineAction env manager mentionRequest = convertResults <$> getUserTimeline env manager mentionRequest
  where convertResults = bimap (\e -> TweetRetrievalError (Mention $ twitterHandler mentionRequest) (TwitterEndpoint endpoint) (TwitterError e)) (TweetOutput (Mention $ twitterHandler mentionRequest) )
  
twitterHandler :: MentionRequest -> String
twitterHandler (MentionRequest (TwitterHandle twuser) _) = "@" <> C8.unpack twuser